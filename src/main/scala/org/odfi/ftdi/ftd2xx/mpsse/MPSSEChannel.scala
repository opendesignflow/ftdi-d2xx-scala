package org.odfi.ftdi.ftd2xx.mpsse

import org.odfi.indesign.core.harvest.HarvestedResource
import org.odfi.ftdi.ftd2xx.FTDIChannel

class MPSSEChannel(val ftdi: FTDIChannel) extends HarvestedResource {

    def getId = ftdi.getId

    // Enable MPSSE
    //------------------

    /**
     * Reset etccc
     */
    def enableMPSSE = {

        // Reset
        ftdi.resetDevice
        ftdi.setNoCharsEvents
        ftdi.setTimeouts(2000, 2000)
        ftdi.setUSBParameters(65536, 65536)

        ftdi.setBitModeReset
        ftdi.setBitModeMPSSE

        // Send a Wrong comand and get 2 bytes read back

    }
    
    def stop = {
        ftdi.setBitModeReset
    }

    // IO Pins States
    //-----------------------
    class Port {

        class Pin {

            // 0 is input, 1 output
            var dir = 0
            var value = 0

            def output = dir = 1
            def input = dir = 0

            def logic1 = {
                dir = 1
                value = 1
            }

            def logic0 = {
                dir = 1
                value = 0
            }
        }

        val pins = (0 until 8).map(i => new Pin).toArray
        var pinsMapping = Map[String, Pin]()

        /**
         *  Select by index
         */
        def apply(index: Int) = pins(index)

        /**
         * Select by name mapping
         */
        def apply(name: String) = {
            pinsMapping.get(name.trim().toLowerCase()) match {
                case Some(p) => p
                case None =>
                    sys.error("No pin mapping defined for name: " + name)
            }
        }

        def mapPin(index: Int, name: String) = {
            this.pinsMapping = this.pinsMapping + (name.trim.toLowerCase() -> this(index))
        }

        // Write to device
        def write(delayRepeat: Int) = {

            var dir: Byte = (this(0).dir |
                (this(1).dir << 1) |
                (this(2).dir << 2) |
                (this(3).dir << 3) |
                (this(4).dir << 4) |
                (this(5).dir << 5) |
                (this(6).dir << 6) |
                (this(7).dir << 7)) match {
                    case minus if (minus < 0) => (~minus).toByte
                    case other                => other.toByte
                }

            var value: Byte = (this(0).value |
                (this(1).value << 1) |
                (this(2).value << 2) |
                (this(3).value << 3) |
                (this(4).value << 4) |
                (this(5).value << 5) |
                (this(6).value << 6) |
                (this(7).value << 7)) match {
                    case minus if (minus < 0) => (~minus).toByte
                    case other                => other.toByte
                }
            (0 until delayRepeat).foreach {
                i =>
                    setGPIO(dir, value)
            }

        }
        
   

    }

    val portA = new Port
     

    def setGPIO(dir: Byte, values: Byte) {

        var commands = Array[Byte](0x80.toByte.toByte, values, dir)
        ftdi.write(commands)
    }

    def setAllGPIOInput = {
        setGPIO(0x00, 0x00)
    }

    // I2C
    //----------------

    def i2cInit = {
        
        //Ensure disable clock divide by 5 for 60Mhz master clock
        //Ensure turn off adaptive clocking
        //ftdi.write(Array[Byte](0x8A.toByte,0x97.toByte))
        var bytes = Array[Byte](
                0x8B.toByte, //Ensure Enable clock divide by 5 for 60Mhz master clock
                0x97.toByte,//Ensure turn off adaptive clocking
                0x8C.toByte, //Enable 3 phase data clock, used by I2C to allow data on both clock edges
        )
        ftdi.write(bytes)
        
        
        // Put SDA and SCL in high
        
        portA("sda").logic1
        portA("scl").logic1
        portA("sdain").input
        portA.write(1)

        // Setup clock
        // The SK clock frequency can be worked out by below algorithm with divide by 5 set as off
		// SK frequency  = 60MHz /((1 +  [(1 +0xValueH*256) OR 0xValueL])*2)
        var targetFreq = 100000
        var divValue = (60000000 / (targetFreq*2)) -1
        
        divValue = 0x003F
        var divValueL = (divValue & 0xFF).toByte
        var divValueH = ((divValue>>8) & 0xFF).toByte
        
        bytes = Array[Byte](
                0x86.toByte, //Command to set clock divisor
               divValueL,//Set 0xValueL of clock divisor
               divValueH, //Set 0xValueH of clock divisor
        )
        ftdi.write(bytes)
        
        // Make sure Loop Back is turned off
        bytes = Array(0x85.toByte)
        ftdi.write(bytes)
        
        // Wait a bit
        Thread.sleep(200)
    
    }
    
    //-- Byte level
    def i2cStart = {

        ftdi.withBufferedWrites {
            
            
             // Set SDA/SCL Open Drain by setting input
        portA("sda").logic1
            portA("scl").logic1
            portA.write(1)
    
            // SDA Low
            portA("sda").logic0
            portA.write(4)
    
            // SCL low
            portA("scl").logic0
            portA.write(4)
        }
       
        
    }

    def i2cStop = {
        
        ftdi.withBufferedWrites {
            
            
        // SCL Open Drain High
        portA("sda").logic0
        portA.write(10)
        portA("scl").logic1
        portA.write(4)
        
        // SDA  Open Drain High
        portA("sda").logic1
        portA.write(4)
        
        
        
        }

    }
 
    
    def i2cSendByte(b:Byte) = {
        
        
            
         this.ftdi.withBufferedWrites {
             
            /*this.portA("sda").logic1
            this.portA("sdain").input
            this.portA.write(1)*/
        
            // Clock out one byte
            ftdi.write(Array[Byte](
            0x11,
            0,
            0,
            b
            ))

            // Set SDA input
            this.portA("sda").input
            this.portA("sdain").input
            this.portA.write(4)

            
            // Send command to scan bits on Rising edge of SCL
             ftdi.write(Array[Byte](
            0x22.toByte,
            0x00.toByte, // 1 bit
            ))
            
            // Send command to flush right away
             ftdi.write(Array[Byte](
            0x87.toByte
            ))
             
         }
         this.portA("sda").logic1
         this.portA.write(4)
    }
    
    def i2cReadAck = {
        var readByte =  ftdi.readOneByte.get
        
       // logFine(s"ACK: "+readByte.toBinaryString)
        
         (readByte&0x1).toInt match {
            case 1 => false
            case 0 => true
        }
    }
    def i2cRepeatStart = {
        
        // Toggle SDA
       
        portA("sda").logic1
        portA.write(5)
         portA("scl").logic1
         portA.write(5)
         
         portA("sda").logic0
        portA.write(5)
        
        portA("scl").logic0
        portA.write(5)
        
    }

    /**
     * Makes the start and additional repeat starts with just the device address
     */
    def i2cDeviceWakeup(addr:Byte,count:Int) = {
        var res = 0
       // this.ftdi.withBufferedWrites {
        i2cStart
            
        // Send the Address byte multiple times to wake up the device
        (0 until count).foreach {
            i => 
                //logFine(s"Wake up: "+i)
             i2cSendByte((addr).toByte)
            
             if (i<count-1) 
                 i2cRepeatStart
        }
           
       // }
    }
    
   
    
    def i2cDeviceWriteBytes(addr: Byte, data: Array[Byte],wakeupCount : Int = 1) = {

        logFine(s"--- Write Reg ---")
         this.ftdi.withBufferedWrites {
             
            /*portA("ext-trig").logic1
             portA.write(100)
             portA("ext-trig").logic0
             portA.write(4)*/
             
            i2cDeviceWakeup((addr<<1).toByte,wakeupCount)
            //i2cRepeatStart
             
             
             // Send Data
             //i2cSendByte((addr<<1).toByte)
             data.foreach {
                 b => 
                     i2cSendByte(b)
             }
              ///i2cSendBytes(Array((addr<<1).toByte)++data)
              
             i2cStop
         }
         
         (0 until wakeupCount) foreach {
             i => 
                 val ack = i2cReadAck
                 logFine[MPSSEChannel](s"Wakeup ACK: "+ack)
           //logFine(s"ACK: "+i2cReadAck)
         }
         
         ( 0 until data.size) foreach {
             i => 
                 
                 val ack = i2cReadAck
              logFine[MPSSEChannel](s"Data ACK: "+ack)
         }
          logFine[MPSSEChannel](s"- Available: "+ftdi.availableBytes)

            
       
    }
    
    def i2cDeviceReadBytes(addr:Byte,count:Int,wakeupCount : Int = 1,trigger:Boolean = false) = {
        
         logFine[MPSSEChannel](s"--- Read From Device---")
         logFine[MPSSEChannel](s"- Available: "+ftdi.availableBytes)
          var raddr = (((addr<<1) | 0x1) & 0xFF).toByte
          
        //  i2cDeviceWakeup(raddr.toByte,wakeupCount)
          
         this.ftdi.withBufferedWrites {
             
             if (trigger) {
                      portA("ext-trig").logic1
             portA.write(100)
             portA("ext-trig").logic0
             portA.write(4)
             }
         
             
             // Wake up using Write command
             wakeupCount match {
                 case 0 => sys.error("Must have at least one wake up")
                 case 1 => 
                     
                      // Last wake up is the Read address
                      i2cDeviceWakeup(raddr.toByte,1)
                      
                 case other => 
                     
                     i2cDeviceWakeup((addr<<1).toByte,wakeupCount-1)
                     i2cRepeatStart
             
                      // Last wake up is the Read address
                     i2cDeviceWakeup(raddr.toByte,1)
             }
             
            
             
             
             // Read Data
             
             i2cReadBytes(count)
              
            if (trigger) {
                portA("ext-trig").logic1
                portA.write(2)
            }
             
             i2cStop
         }
        
         
         logFine[MPSSEChannel](s"- Available: "+ftdi.availableBytes)
        (0 until wakeupCount) foreach {
             i => 
                  val ack = i2cReadAck
                 logFine[MPSSEChannel](s"Wakeup ACK: "+ack)
                 logFine[MPSSEChannel](s"- Available: "+ftdi.availableBytes)
           //logFine(s"ACK: "+i2cReadAck)
         }
        // logFine(s"Read Address ACK: "+i2cReadAck)
         //logFine(s"Read Address ACK: "+i2cReadAck)
           // THre should be count bytes now in the input buffer
         //println(s"- Available: "+ftdi.availableBytes)
        var readBytes = ftdi.readBytes(count)
         
        logFine[MPSSEChannel](s"REad bytes: "+readBytes.toList.map(_.toHexString))
        //println(s"- Available: "+ftdi.availableBytes)
        logFine[MPSSEChannel](s"- Available: "+ftdi.availableBytes)
        readBytes
    }
    
    def i2cReadBytes(count:Int) = {
        
        //this.portA("sda").input
        //this.portA.write(1)
        
        this.ftdi.withBufferedWrites {
                
            (0 until count).foreach {
                i => 
                    
                    
                this.portA("sda").input
                 this.portA("sdain").input
                this.portA.write(1)
          
                // Command to read a byte
                  ftdi.write(Array[Byte](
                    0x20.toByte,
                    0x00.toByte, 0x00.toByte // 1 byte
                ))
                
                // Send command to flush right away
                ftdi.write(Array[Byte](
                    0x87.toByte
                ))
                
                // SDA is held low to generate ACK
                // clock out one bit on next -ve
                this.portA("sda").logic0
                this.portA.write(1)
                ftdi.write(Array[Byte](
                    0x1B.toByte,
                    0x00.toByte, 0x00.toByte // 1 bit
                ))
                
            }
        }
        
      
    }
    
    // SPI
    //------------
    
    def spiInit = {
        
        //Ensure disable clock divide by 5 for 60Mhz master clock
        //Ensure turn off adaptive clocking
        //ftdi.write(Array[Byte](0x8A.toByte,0x97.toByte))
        var bytes = Array[Byte](
                0x8B.toByte, //Ensure Enable clock divide by 5 for 60Mhz master clock
                0x97.toByte,//Ensure turn off adaptive clocking
                0x8D.toByte, //Disable 3 phase data clock, used by I2C to allow data on both clock edges
        )
        ftdi.write(bytes)
        
        
        // Put MOSI in 0
        // Put MISO in input
        // Put SCLK in 0
        // Put SSL in 1
        
        portA("mosi").logic0
        portA("sclk").logic0
        portA("ssel").logic1
        portA("miso").input
        
        portA.write(1)

        // Setup clock
        // The SK clock frequency can be worked out by below algorithm with divide by 5 set as off
		// SK frequency  = 60MHz /((1 +  [(1 +0xValueH*256) OR 0xValueL])*2)
        var targetFreq = 100000
        var divValue = (60000000 / (targetFreq*2)) -1
        
        divValue = 0x003F
        var divValueL = (divValue & 0xFF).toByte
        var divValueH = ((divValue>>8) & 0xFF).toByte
        
        bytes = Array[Byte](
                0x86.toByte, //Command to set clock divisor
               divValueL,//Set 0xValueL of clock divisor
               divValueH, //Set 0xValueH of clock divisor
        )
        ftdi.write(bytes)
        
        // Make sure Loop Back is turned off
        bytes = Array(0x85.toByte)
        ftdi.write(bytes)
        
        // Wait a bit
        Thread.sleep(200)
        
    }
    
    def spiSend2BytesFrame(a:Byte,b:Byte) {
        
        this.ftdi.withBufferedWrites {
            
            // Toggle SSEL
            try {
                portA("ssel").logic0
                portA.write(1)
            
                
                // Clock out 2 bytes, high order first! Low Endian!
                ftdi.write(Array[Byte](
                0x11,
                1,
               0,
               b,
                a
                ))
               
               /*  ftdi.write(Array[Byte](
                0x11,
                0,
                0,
                b
                ))*/
            
            
            } finally {
                portA("ssel").logic1
                portA.write(4)
            
            }
        }
        
        println(s"Done Sending byte")
        
    }
    
    

}