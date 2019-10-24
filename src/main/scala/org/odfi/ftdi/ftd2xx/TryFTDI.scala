package org.odfi.ftdi.ftd2xx

object TryFTDI extends App {

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
            println("A Openend: "+aChannel.node.ftHandle())
            aChannel.ensureOpened
            println("A Openend: "+aChannel.node.ftHandle())
            var mpsse = aChannel.toMPSSE
            mpsse.enableMPSSE
            mpsse.setAllGPIOInput
            
            // Make SPI Channel
            
    }

}