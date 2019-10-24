package org.odfi.ftdi.ftd2xx

import org.odfi.cypress.CY8CMBR3102
import org.odfi.indesign.core.brain.Brain
import org.odfi.ftdi.ftd2xx.mpsse.MPSSEChannel

object TryFTDII2C extends App {

    FTDIHarvester.harvest

    println(s"Count Channels: " + FTDIHarvester.getResources.size)

    /*FTDIHarvester.getResourcesOfType[FTDIChannel].foreach {
        chan =>
            println("Channel: "+chan.getDeviceID)
            println("Type: "+chan.getType)
    }*/

    FTDIHarvester.getResourcesOfType[FTDIDevice].foreach {
        d =>
            println("Device: " + d.getId)
            d.getDerivedResources[FTDIChannel].foreach {
                chan =>
                    println("Channel: " + chan.getDeviceID)
                    println("Type: " + chan.getType)
            }
            // println("Type: " + chan.getType)

            // Get A
            var aChannel = d.getAChannel.get
            println("A Openend: " + aChannel.node.ftHandle())
            aChannel.ensureOpened
            println("A Openend: " + aChannel.node.ftHandle())
            var mpsse = aChannel.toMPSSE
            mpsse.enableMPSSE
            mpsse.setAllGPIOInput

            // Map pins
            mpsse.portA.mapPin(7,"ext-trig")
            mpsse.portA.mapPin(0, "scl")
            mpsse.portA.mapPin(1, "sda")
            mpsse.portA.mapPin(2, "sdain")

            /*mpsse.portA("scl").logic1
            mpsse.portA("sda").logic1
            mpsse.portA.write(2)
            */
            //mpsse.ftdi.write(Array(0x9E.toByte,0xFF.toByte,0xFF.toByte))
            // Init
            mpsse.i2cInit
            
            
            mpsse.portA("ext-trig").logic1
            mpsse.portA.write(2)
        

            println(s"---------------")

            //val chip = new CY8CMBR3102(mpsse)

            //chip.readID

            // mpsse.i2cDeviceWakeup(0x38, 2)
            // mpsse.i2cWriteRegister(addr, data, preWakeUp)

            var addr = 0x37.toByte
            
           /* mpsse.i2cDeviceWriteBytes(addr, Array(0x00.toByte, 0x00.toByte), wakeupCount = 4)
            Thread.sleep(100)

            mpsse.i2cDeviceWriteBytes(addr, Array(0x4c.toByte, 0x00.toByte), wakeupCount = 4)
            Thread.sleep(100)

            mpsse.i2cDeviceWriteBytes(addr, Array(0x40.toByte, 0x01.toByte), wakeupCount = 4)
            Thread.sleep(100)

            mpsse.i2cDeviceWriteBytes(addr, Array(0x80.toByte, 0x1.toByte), wakeupCount = 4)
            Thread.sleep(100)
            
            mpsse.i2cDeviceWriteBytes(addr, Array(0x86.toByte, 0x2.toByte), wakeupCount = 4)
            Thread.sleep(500)
             mpsse.i2cDeviceWriteBytes(addr, Array(0x86.toByte, 0xff.toByte), wakeupCount = 4)
            
            */
            
            //Brain.tlogEnableFull[MPSSEChannel]
            
            
           mpsse.i2cDeviceWriteBytes(addr, Array(0x8f.toByte), wakeupCount = 4)
           // mpsse.i2cDeviceWriteBytes(addr, Array(0x55.toByte), wakeupCount = 4)
            mpsse.i2cDeviceReadBytes(addr,1,wakeupCount=4)
             
            
            
            //  mpsse.i2cDeviceReadBytes(addr,2,wakeupCount=2)

            //mpsse.i2cReadBytes(2)
            // println(s"Found: "+mpsse.ftdi.readBytes(2).toList.map(_.toBinaryString))

            //mpsse.i2cR
            /*var addr = 0x37.toByte
            //mpsse.ftdi.withBufferedWrites {
            mpsse.i2cWriteRegister(addr, Array(0x00))
            mpsse.i2cWriteRegister(addr, Array(0x00))
            mpsse.i2cWriteRegister(addr, Array(0x00))*/

            // Write 1 to GPO CFG 0x40
            // ->

            //  mpsse.i2cWriteRegister(addr, Array(0x00))
            //  mpsse.i2cWriteRegister(addr, Array(0x00))
            //}
            // mpsse.i2cStart

            //mpsse.i2cSendBytes(Array(  (0x37<<1).toByte))

            // mpsse.i2cStop

            // mpsse.setAllGPIOInput

            mpsse.stop

    }

}