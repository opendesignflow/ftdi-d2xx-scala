package org.odfi.cypress

import org.odfi.ftdi.ftd2xx.mpsse.MPSSEChannel
import org.odfi.ic.register.RegisterSource
import scala.language.dynamics

class CY8CMBR3102(val mpsse: MPSSEChannel) {

    var deviceAddress = 0x37.toByte
    var wakeupCalls = 8
    

    def open = {

    }

    // IDs and so on
    //----------

    def readFamilyID: Int = {

        // Write Pointer
        mpsse.i2cDeviceWriteBytes(deviceAddress, Array(0x8f.toByte), wakeupCalls)

        // Read
        val familyID = (mpsse.i2cDeviceReadBytes(deviceAddress, 1, wakeupCalls)(0) & 0xff).toInt

        println(s"F: " + familyID.toHexString)
        familyID

    }

    def readDeviceID: Int = {

        // Write Pointer
        mpsse.i2cDeviceWriteBytes(deviceAddress, Array(0x90.toByte), wakeupCalls)

        // Read
        var idmsblsb = mpsse.i2cDeviceReadBytes(deviceAddress, 2, wakeupCalls, trigger = true)

        println("DID: " + idmsblsb.toList.map(_.toHexString))
        val deviceId = ((idmsblsb(0) | (idmsblsb(1) << 8)) & 0xFFFF)

        println(s"D: " + deviceId.toHexString)

        deviceId

    }

    def readFirmwareRevision: Int = {

        // Write Pointer
        mpsse.i2cDeviceWriteBytes(deviceAddress, Array(0x92.toByte), wakeupCalls)

        // Read
        var idmsblsb = mpsse.i2cDeviceReadBytes(deviceAddress, 2, wakeupCalls)

        println("DID: " + idmsblsb.toList.map(_.toHexString))
        val deviceRev = (idmsblsb(1) & 0xFF)

        println(s"DR: " + deviceRev.toHexString)

        deviceRev

    }

    def write(regAddr: Int, value: Byte): Unit = {
        //println(s"Write to: "+regAddr.toHexString+"//->"+value.toHexString)
        mpsse.i2cDeviceWriteBytes(deviceAddress, Array(regAddr.toByte, value), wakeupCalls)
    }

    def write2(regAddr: Int, value: Int): Unit = {
        // println(s"Write 2  to: "+regAddr.toHexString+"//->"+value.toHexString)
        mpsse.i2cDeviceWriteBytes(deviceAddress, Array(regAddr.toByte, (value & 0xFF).toByte, (((value) >> 8) & 0xFF).toByte), wakeupCalls)
    }

    def readByte(regAddr: Int) = {

        //println(s"Read to: "+regAddr.toHexString)

        // Write Pointer
        mpsse.i2cDeviceWriteBytes(deviceAddress, Array(regAddr.toByte), wakeupCalls)

        // Read
        var b = mpsse.i2cDeviceReadBytes(deviceAddress, 1, wakeupCalls, trigger = true)
        (b(0) & 0xff)

    }

    def readFully = {
        // Write Pointer
        mpsse.i2cDeviceWriteBytes(deviceAddress, Array(0x00), wakeupCalls)

        var bytes = mpsse.i2cDeviceReadBytes(deviceAddress, 128, wakeupCalls, trigger = true)

        bytes.map(b => (b & 0xFF).toByte)

    }

    /*def writeFully(bytes: Array[Byte]) = {
        assert(bytes.size == 128, "Memory of chip needs 128 bytes")

        mpsse.i2cDeviceWriteBytes(deviceAddress, bytes, wakeupCalls)
    }*/

    def updateCRC = {
        val crc = CY8CMBR3102.crcOf(126)
        write2(CY8CMBR3102.CONFIG_CRC, crc)
    }
    
    def updateRegister(addr:Int,value:Int) = {
        
        CY8CMBR3102.registerSize(addr) match {
            case 1 => 
                this.write(addr,value.toByte)
            case other => 
                this.write2(addr,value)
        }
        
        if (addr<126) {
            CY8CMBR3102.setRegisterValue(addr,value)
        }
    }
    
    def saveConfig = {
        updateCRC
        write(CY8CMBR3102.CTRL_CMD,0x02)
        Thread.sleep(200)
        write(CY8CMBR3102.CTRL_CMD,0xff.toByte)
        Thread.sleep(200)
    }
    
    def sendReset = {
        write(CY8CMBR3102.CTRL_CMD,0xff.toByte)
        Thread.sleep(200)
    }

}

object CY8CMBR3102 extends RegisterSource with Dynamic {

    loadRegistersFrom(getClass.getClassLoader.getResource(getClass.getPackage.getName.replace(".", "/") + "/Regs.txt"))

    /**
     * Returns address
     */
    def selectDynamic(name: String): Int = {
        this.nameAddressMap(name)._1
    }
    
    

}