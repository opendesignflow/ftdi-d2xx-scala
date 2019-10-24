package org.odfi.ftdi.ftd2xx

import org.bridj.Pointer
import org.odfi.indesign.core.harvest.Harvester
import scala.collection.JavaConverters
import scala.collection.JavaConverters._
import org.odfi.indesign.core.harvest.HarvestedResourceDefaultId
import org.odfi.indesign.core.harvest.HarvestedResource
import org.odfi.ftdi.ftd2xx.mpsse.MPSSEChannel
import org.odfi.ftdi.ftd2xx.mpsse.MPSSEChannel
import java.nio.ByteBuffer

object FTDIHarvester extends Harvester {

    def countDevices = {

        var countPointer = Pointer.allocate(classOf[Long])
        Ftd2xxLibrary.FT_ListDevices(countPointer, null, Ftd2xxLibrary.FT_LIST_NUMBER_ONLY)

        countPointer.get()

    }

    override def doHarvest = {

        // Create Info List
        //-------------------------
        var countPointer = Pointer.allocate(classOf[Integer])
        Ftd2xxLibrary.FT_CreateDeviceInfoList(countPointer)

        // Get Devices Info List
        //-----------------------
        var deviceNodes = Pointer.allocateArray(classOf[FT_DEVICE_LIST_INFO_NODE], countPointer.get().toLong)
        Ftd2xxLibrary.FT_GetDeviceInfoList(deviceNodes, countPointer)

        // Group Channels by Device
        //----------------
        if (countPointer.get() > 0) {
            var deviceNodesList = deviceNodes.asList().asScala.toList
            deviceNodesList.zipWithIndex.map {
                case (node, index) =>
                    new FTDIChannel(index, node)
            }.groupBy(_.getDeviceID).map {
                case (did, channels) => new FTDIDevice(did, channels)
            }.foreach(gather(_))
        }

    }

    def getFirstFTDIDevice = this.getResourcesOfType[FTDIDevice].headOption

}

class FTDIDevice(val deviceId: Int, val channels: List[FTDIChannel]) extends HarvestedResource {

    def getId = deviceId.toString

    this.onGathered {
        case h if (h == FTDIHarvester) =>
            channels.foreach {
                c =>
                    this.addDerivedResource(c)
            }
    }

    def getAChannel = this.findDerivedResourceOfTypeAnd[FTDIChannel](c => c.getSerialNumber.endsWith("A"))
    def getBChannel = this.findDerivedResourceOfTypeAnd[FTDIChannel](c => c.getSerialNumber.endsWith("B"))

}

class FTDIChannel(val index: Int, val node: FT_DEVICE_LIST_INFO_NODE) extends HarvestedResourceDefaultId {

    override def getId = getSerialNumber

    def getSerialNumber = node.SerialNumber().getCString

    def getType = node.Type()

    def getDeviceID = node.ID()

    this.onClean {
        this.close
    }

    // Open/Close
    //---------------
    var isOpened = false
    def ensureOpened = isOpened match {
        case true =>
            node.ftHandle()
        case false =>

            node.synchronized {

                var untyped = node.ftHandle().asInstanceOf[Pointer[_]]
                var pointerToPointer = Pointer.pointerToPointer(untyped).asInstanceOf[Pointer[Pointer[_]]]
                Ftd2xxLibrary.FT_Open(index, pointerToPointer) match {
                    case Ftd2xxLibrary.FT_OK =>
                        node.ftHandle(pointerToPointer.get)
                        isOpened = true
                        node.ftHandle()
                    case other =>
                        sys.error("Could not open device: " + getSerialNumber)

                }
            }

    }

    def close = isOpened match {
        case true =>
            Ftd2xxLibrary.FT_Close(node.ftHandle())
        case false =>
    }

    // Controls
    //-----------------

    def resetDevice = {
        Ftd2xxLibrary.FT_ResetDevice(ensureOpened)
        Ftd2xxLibrary.FT_Purge(ensureOpened, Ftd2xxLibrary.FT_PURGE_RX | Ftd2xxLibrary.FT_PURGE_TX)
    }

    def setUSBParameters(ulInTransferSize: Int, ulOutTransferSize: Int) = {

        Ftd2xxLibrary.FT_SetUSBParameters(ensureOpened, ulInTransferSize, ulOutTransferSize) match {
            case Ftd2xxLibrary.FT_OK =>

            case other =>
                sys.error("Could not set USB Parameters on device: " + getSerialNumber)

        }
    }

    def setNoCharsEvents = {
        Ftd2xxLibrary.FT_SetChars(ensureOpened, 0.toByte, 0.toByte, 0.toByte, 0.toByte)
    }

    def setTimeouts(recv: Int, send: Int) = {
        Ftd2xxLibrary.FT_SetTimeouts(ensureOpened, recv, send)
    }

    def setLatencyTimer(timer: Int) = {
        assert(timer >= 0)
        Ftd2xxLibrary.FT_SetLatencyTimer(ensureOpened, timer.toByte)
    }

    // Bit Modes
    //------------------
    def setBitMode(mask: Int, mode: Int) = {
        assert(mask >= 0)
        assert(mode >= 0)
        Ftd2xxLibrary.FT_SetBitMode(ensureOpened, mask.toByte, mode.toByte) match {
            case Ftd2xxLibrary.FT_OK =>

            case other =>
                sys.error(s"Could not set Bit Mode $mode  on device: " + getSerialNumber)

        }
    }
    def setBitModeReset = {
        setBitMode(0, 0)
    }
    def setBitModeMPSSE = {
        setBitMode(0, 2)
    }

    // MPSSE
    //-------------

    def isMPSSE = this.getSerialNumber.endsWith("A") || this.getSerialNumber.endsWith("B")

    def toMPSSE = this.findDerivedResourceOfType[MPSSEChannel] match {
        case Some(mpsse)        => mpsse
        case None if (!isMPSSE) => sys.error("Cannot Use Channel as MPSSE")
        case None =>

            this.addDerivedResource(new MPSSEChannel(this))

    }

    // Read/Write
    //-------------------
    var bufferedWrites = false
    var buf = List[Byte]()

    def enableBufferWrites = {
        bufferedWrites = true
    }
    def disableBufferWrites = {
        bufferedWrites = true
    }
    def clearWriteBuffer = {
        buf = List[Byte]()
    }
    def commitBufferedWrites = {
        buf.size match {
            case 0 =>
            case other =>
                logFine[FTDIChannel](s"Writing Buffered command " + buf(0).toHexString + " of: " + buf.size)
                logFine[FTDIChannel]("Bytes: " + buf.toList.map(_.toHexString))
                var pointerToBuffer = Pointer.pointerToBytes(ByteBuffer.wrap(buf.toArray))
                var written = Pointer.allocateInt()
                Ftd2xxLibrary.FT_Write(ensureOpened, pointerToBuffer, buf.size, written)

                logFine[FTDIChannel]("Written: " + written.get + " from " + buf.size)
        }

        clearWriteBuffer
    }

    def write(bytes: Array[Byte]) = this.synchronized {

        bufferedWrites match {
            case true =>
                buf = buf ++ bytes
            case false =>
                logFine[FTDIChannel](s"Writing command " + bytes(0).toHexString + " of: " + bytes.size)
                logFine[FTDIChannel]("Bytes: " + bytes.toList)
                var pointerToBuffer = Pointer.pointerToBytes(ByteBuffer.wrap(bytes))
                var written = Pointer.allocateInt()
                Ftd2xxLibrary.FT_Write(ensureOpened, pointerToBuffer, bytes.size, written)

                logFine[FTDIChannel]("Written: " + written.get + " from " + bytes.size)
        }

    }

    /**
     * Buffers writes during Closure, turn off buffering after
     */
    def withBufferedWrites(cl: => Any) = {

        if (bufferedWrites) {
            cl
        } else {
            bufferedWrites = true
            clearWriteBuffer
            try {
                cl
                commitBufferedWrites
            } finally {
                bufferedWrites = false
                clearWriteBuffer
            }
        }

    }

    // Read
    //---------------

    def availableBytes = {
        var readCount = Pointer.allocateInt()
        var writeCount = Pointer.allocateInt()
        var evCount = Pointer.allocateInt()
        //Ftd2xxLibrary.FT_GetQueueStatus(ensureOpened, readCount)
        Ftd2xxLibrary.FT_GetStatus(ensureOpened, readCount, writeCount, evCount)
        readCount.get()
    }
    def readBytes(count: Int): Array[Byte] = {

        logFine[FTDIChannel](s"Reading $count bytes")

        var readCount = Pointer.allocateInt()
        var inBuf = Pointer.allocateArray(classOf[Byte], count)
        Ftd2xxLibrary.FT_Read(ensureOpened, inBuf, count, readCount)

        logFine[FTDIChannel](s"-> Read ${readCount.get()} bytes")

        if (readCount.get() < count) {
            sys.error("Bytes not read fully")
        }

        inBuf.getBytes(readCount.get)

    }
    def readOneByte: Option[Byte] = {

        var readCount = Pointer.allocateInt()
        var inBuf = Pointer.allocateArray(classOf[Byte], 1)
        Ftd2xxLibrary.FT_Read(ensureOpened, inBuf, 1, readCount)

        readCount.get().toInt match {
            case 0 =>
                None
            case 1 =>
                Some(inBuf.get())
        }

    }

}