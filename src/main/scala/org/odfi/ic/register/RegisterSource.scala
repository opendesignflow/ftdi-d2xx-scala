package org.odfi.ic.register

import java.net.URL
import com.idyria.osi.tea.io.TeaIOUtils
import scala.io.Source
import java.io.File
import java.io.PrintStream
import java.io.FileOutputStream
import com.idyria.osi.tea.hash.CCITTCRC16

trait RegisterSource {

    var nameAddressMap = Map[String, (Int, Int)]()
    var nameAddressList = List[(String, (Int, Int))]()

    var currentValues = Array[Byte]()

    def hasValuesDefined = {
        currentValues.size match {
            case 0     => true
            case other => false
        }
    }

    def clearValues = {
        this.currentValues = Array[Byte]()
    }
    
    def setCurrentValues(bytes: Array[Byte]) = {
        currentValues = bytes
    }

    def setRegisterValue(name: String, value: Int): Unit = {

        registerSize(name) match {
            case 1 =>
                this.currentValues = this.currentValues.updated(registerAddress(name), value.toByte)
            case other =>
                val b0 = (value & 0xFF).toByte
                val b1 = ((value >> 8) & 0xFF).toByte
                println(s"Reg valueo f size: " + other + " -> " + b0 + "//->" + b1)
                this.currentValues = this.currentValues.updated(registerAddress(name), b0)
                this.currentValues = this.currentValues.updated(registerAddress(name) + 1, b1)

        }

    }
    def getRegisterValue(name: String) = {

        registerSize(name) match {
            case 1 =>
                this.currentValues(registerAddress(name)) & 0xFF
            case other =>
                val bytes = this.currentValues.drop(registerAddress(name)).take(2)
                val b0 = bytes(0)
                val b1 = bytes(1)
                ((((b1 << 8) & 0xFF00) | (b0 & 0xFF)) & 0xFFFF)

        }

    }

    def setRegisterValue(addr: Int, value: Int): Unit = {

        registerSize(addr) match {
            case 1 =>
                this.currentValues = this.currentValues.updated(addr, value.toByte)
            case other =>
                val b0 = (value & 0xFF).toByte
                val b1 = ((value >> 8) & 0xFF).toByte
                println(s"Reg valueo f size: " + other + " -> " + b0 + "//->" + b1)
                this.currentValues = this.currentValues.updated(addr, b0)
                this.currentValues = this.currentValues.updated(addr + 1, b1)

        }

    }

    def registerAddress(name: String) = {
        this.nameAddressMap(name)._1
    }

    def registerSize(name: String): Int = {
        this.nameAddressMap(name)._2
    }

    def registerSize(addr: Int): Int = {

        this.nameAddressList.find {
            case (name, (raddr, size)) => raddr == addr
        } match {
            case Some((name, (raddr, size))) => size
            case None                        => sys.error("Could not find register with addr: " + addr)
        }

    }

    def loadRegistersFrom(url: URL) = {

        val data = TeaIOUtils.swallowStream(url.openStream())

        // Make the map with the Name and addresses
        val nap = Source.fromURL(url, "UTF-8").getLines().map {
            line =>

                val split = line.trim.split(" ")
                (split.head.trim, Integer.parseInt(split.last.trim.replace("0x", ""), 16).toInt)
        }.toList

        // Make a second pass and find out the numer of bytes of every register
        val napWithSize = nap.zipWithIndex.map {

            case ((name, addr), i) =>

                val byteSize = i match {
                    case last if (i == nap.size - 1) => 2
                    case other =>

                        val nextAddress = nap(other + 1)._2

                        (nextAddress - addr)
                }

                (name, (addr, byteSize))

        }

        nameAddressList = napWithSize.toList
        nameAddressMap = napWithSize.toMap

    }

    def crcOf(size: Int) = {
        val crc = CCITTCRC16.calc(currentValues.take(size))
        println(s"CRC: " + crc)
        crc
    }

    def saveRegisterValues(out: File) = {

        var os = new PrintStream(new FileOutputStream(out))

        var bytesIndex = 0
        this.nameAddressList.foreach {
            case (name, (addr, size)) if (addr < currentValues.size) =>

                println(s"Addre: $addr , size=$size, current=$bytesIndex")

                var value = size match {
                    case 1 =>
                        val b = currentValues(bytesIndex)
                        bytesIndex = bytesIndex + 1
                        b & 0xFF
                    case 2 =>
                        val b0 = currentValues(bytesIndex)
                        val b1 = currentValues(bytesIndex + 1)
                        bytesIndex = bytesIndex + 2
                        /*val b1i = b1.toInt
                        val b1adapt = (b1i<<8) & 0xFF00
                        val b0i =b0.toInt& 0xFF
                        println(s"B1; "+b1adapt.toHexString)
                        println(s"B1; "+(b1adapt | b0i).toHexString)
                        (b1adapt | b0i)*/

                        ((b1 << 8) & 0xFF00) | (b0 & 0xFF)
                    case other =>
                        val b = currentValues(bytesIndex)
                        bytesIndex = bytesIndex + other
                        b & 0xFF

                }
                os.println("0x" + value.toHexString + s" // $name ($size@$addr): ${value.toBinaryString} ")

            case other =>
        }
        /*
        bytes.zipWithIndex.foreach {
            case (b,i) =>

                val (name,(addr,size)) = this.nameAddressList(i)

                os.println("0x"+b.toHexString+s" // $name: ${b.toBinaryString} ")

        }*/

        os.close

    }

}