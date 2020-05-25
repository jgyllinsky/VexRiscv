package flogics.lib.axi4

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi.{Axi4Config, Axi4ReadOnly}
import spinal.lib.com.spi.SpiMaster

object Axi4ReadOnlyConfig {
  // Axi4CrossbarFactory acceptable
  def get(byteCount: BigInt) =
    Axi4Config(
      addressWidth = log2Up(byteCount),
      dataWidth = 32,
      idWidth = 4,
      useId = true,
      useRegion = false,
      useBurst = true,
      useLock = false,
      useCache = false,
      useSize = true,
      useQos = false,
      useLen = true,
      useLast = true,
      useResp = false,
      useProt = false,
      useStrb = false
    )
}

case class At25sf081Generics(
    widthCmd: Int = 8,
    widthAddr: Int = 24,
    widthData: Int = 8,

    cmdReadArray: Int = 0x3,
    cmdReadId: Int = 0x9f,
    cmdResume: Int = 0xab,
    cmdResumeReadId: Int = 0xab
)

class Axi4TestRom() extends Component {
  val io = new Bundle {
    val axi = slave(Axi4ReadOnly(Axi4ReadOnlyConfig.get(byteCount = 32)))
  }

  val ar = io.axi.readCmd
  val r = io.axi.readRsp

  ar.ready := False
  r.valid := False
  r.payload.id := ar.payload.id
  r.payload.last := False
  r.payload.data := B(0, 32 bits)

  val fsm = new StateMachine {
    val idle = new State with EntryPoint
    val reading = new State

    idle
      .whenIsActive {
        when(ar.valid) {
          goto(reading)
        }
      }

    reading
      .whenIsActive {
        ar.ready := True
        r.valid := True
        r.payload.last := True
        r.payload.data := B(ar.payload.addr, 32 bits)
        when(r.ready) {
          goto(idle)
        }
      }
  }
}

case class SpiMasterCtrlCmdSet(
    g: At25sf081Generics = At25sf081Generics()
) extends Bundle with IMasterSlave {
  /*
   * spiCmd: command byte to SPI ROM
   * addr: address word to SPI ROM
   * withAddr: send address word if true
   * lenRead: number of bytes to read from SPI ROM
   *
   * lenRead value (in bytes) can be 256 (max length of AXI4 'len + 1' words)
   * times 4 (32 / 8), or 1024, so we need 11 bits width to hold 1024.
   */
  val spiCmd = Bits(g.widthCmd bits)
  val addr = UInt(g.widthAddr bits)
  val withAddr = Bool
  val lenRead = UInt(log2Up(256 * 4 + 1) bits)

  override def asMaster(): Unit = {
    out(spiCmd, addr, withAddr)
  }
}

class SpiMasterCtrl(
    g: At25sf081Generics = At25sf081Generics()
) extends Component {
  val io = new Bundle {
    /*
     * cmd: input stream of SpiMasterCtrlCmdSet
     * data: output fragment flow of read data
     * spi: SpiMaster interface
     */
    val cmd = slave(Stream(SpiMasterCtrlCmdSet()))
    val data = master(Flow(Fragment(Bits(g.widthData bits))))
    val spi = master(SpiMaster(useSclk = true))
  }

  def shiftedBit(b: Bits, shift: UInt) = ((b >> shift) & 1) === 1

  val sclk = Reg(Bool)
  val valid = Reg(Bool)
  val last = Reg(Bool)
  val lenSend = Reg(UInt(log2Up(g.widthAddr) bits))
  val addr = Reg(UInt(g.widthAddr bits))
  val spiCmd = Reg(Bits(g.widthCmd bits))
  val lenRead = Reg(UInt(io.cmd.payload.lenRead.getWidth bits))
  val withRead = Reg(Bool)
  val withAddr = Reg(Bool)
  val mosi = Reg(Bool)
  val ctBits = Reg(UInt(log2Up(g.widthData) bits))
  val dataShift = Reg(Bits(g.widthData bits))

  io.cmd.ready := False
  io.spi.ss := B(1, 1 bit)

  val fsm = new StateMachine {
    val idle = new State with EntryPoint
    val sendingCmd = new State
    val sendingAddr = new State
    val reading = new State
    val holdingSs = new State

    idle
      .whenIsActive {
        sclk := False
        valid := False
        last := False

        when(io.cmd.valid) {
          lenSend := g.widthCmd - 1
          addr := io.cmd.payload.addr
          spiCmd := io.cmd.payload.spiCmd
          lenRead := io.cmd.payload.lenRead - 1
          withRead := io.cmd.payload.lenRead =/= 0
          withAddr := io.cmd.payload.withAddr
          mosi := shiftedBit(io.cmd.payload.spiCmd, g.widthCmd - 1)
          ctBits := g.widthData - 1

          io.cmd.ready := True
          goto(sendingCmd)
        }
      }

    sendingCmd
      .whenIsActive {
        sclk := !sclk

        io.spi.ss := B(0, 1 bit)

        when(sclk) {
          lenSend := lenSend - 1
          mosi := shiftedBit(spiCmd, lenSend - 1)

          when(lenSend === 0) {
            when(withAddr) {
              lenSend := g.widthAddr - 1
              mosi := shiftedBit(addr.asBits, g.widthAddr - 1)
              goto(sendingAddr)
            } otherwise {
              valid := True
              last := True
              goto(holdingSs)
            }
          }
        }
      }

    sendingAddr
      .whenIsActive {
        sclk := !sclk

        io.spi.ss := B(0, 1 bit)

        when(sclk) {
          lenSend := lenSend - 1
          mosi := shiftedBit(addr.asBits, lenSend - 1)

          when(lenSend === 0) {
            when(withRead) {
              goto(reading)
            } otherwise {
              valid := True
              last := True
              goto(holdingSs)
            }
          }
        }
      }

    reading
      .whenIsActive {
        sclk := !sclk
        valid := False

        io.spi.ss := B(0, 1 bit)

        when(sclk === False) {
          ctBits := ctBits - 1
          dataShift := (dataShift |<< 1) | io.spi.miso.asBits.resized

          when(ctBits === 0) {
            ctBits := g.widthData - 1
            lenRead := lenRead - 1
            valid := True

            when(lenRead === 0) {
              last := True
              goto(holdingSs)
            }
          }
        }
      }

    holdingSs
      .whenIsActive {
        sclk := False
        valid := False
        last := False

        io.spi.ss := B(0, 1 bit)
        goto(idle)
      }
  }

  io.spi.sclk := sclk
  io.spi.mosi := mosi
  io.data.valid := valid
  io.data.fragment := dataShift
  io.data.last := last
}

import spinal.core._
import spinal.sim._
import spinal.core.sim._

object SpiMasterSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.doSim(
      new SpiMasterCtrl()
    ) { dut =>
      def wait(
          count: Int = 1,
          assertion: Boolean = true,
          msg: String = "wait"
      ) {
        for (i <- 0 until count) {
          dut.clockDomain.waitSampling(1)
          assert(assertion, msg)
        }
      }

      dut.clockDomain.forkStimulus(period = 10)

      dut.io.cmd.valid #= false
      dut.io.spi.miso #= false

      /*
       * Only command
       */
      dut.io.cmd.payload.spiCmd #= 0xab
      dut.io.cmd.payload.addr #= 0x123456
      dut.io.cmd.payload.withAddr #= false
      dut.io.cmd.payload.lenRead #= 0

      wait(10)
      dut.io.cmd.valid #= true
      wait(1)
      dut.io.cmd.valid #= false

      while (! dut.io.data.valid.toBoolean)
        wait(1)

      /*
       * With address
       */
      dut.io.cmd.payload.spiCmd #= 0x03
      dut.io.cmd.payload.addr #= 0x123456
      dut.io.cmd.payload.withAddr #= true
      dut.io.cmd.payload.lenRead #= 0
      dut.io.cmd.valid #= true
      wait(1)
      dut.io.cmd.valid #= false

      while (! dut.io.data.valid.toBoolean)
        wait(1)

      /*
       * With address and read
       */
      dut.io.cmd.payload.spiCmd #= 0x03
      dut.io.cmd.payload.addr #= 0x123456
      dut.io.cmd.payload.withAddr #= true
      dut.io.cmd.payload.lenRead #= 4
      dut.io.cmd.valid #= true
      wait(1)
      dut.io.cmd.valid #= false
      dut.io.cmd.payload.spiCmd #= 0xff
      dut.io.cmd.payload.addr #= 0xffffff
      dut.io.cmd.payload.withAddr #= false
      dut.io.cmd.payload.lenRead #= 0

      var ct: Int = 0
      while (ct < 33) {
        while (dut.io.spi.sclk.toBoolean) {
          wait(1)
        }

        wait(1)
        ct += 1
      }

      val word: Long = 0x55aa1234
      for (i <- 0 until 32) {
        dut.io.spi.miso #= (word & (1 << (31 - i))) != 0
        wait(2)
      }

      wait(10)
    }
  }
}

class Axi4RomAt25sf081(
    g: At25sf081Generics = At25sf081Generics(),
    byteCount: BigInt
) extends Component {
  val io = new Bundle {
    val axi = slave(Axi4ReadOnly(Axi4ReadOnlyConfig.get(byteCount)))
    val spi = master(SpiMaster(useSclk = true))
  }
  /*
   * Mapping bus address to SPI ROM address
   *
   * On TinyFPGA BX SPI ROM, user data begins at 0x50000.  So we map bus
   * addresses as following.  Not using an adder.
   *
   * Examples:
   *   0x000124 -> 0x050124
   *   0x010260 -> 0x060260, etc.
   *
   * On the other hand, if MSB of bus address is 1, simply clear the MSB to
   * allow access any range of SPI ROM.
   *
   * Examples:
   *   0x100124 -> 0x000124
   *   0x110260 -> 0x010260, etc.
   *
   */
  def calcAddr(busAddr: UInt) = {
    val partAddr = busAddr(19 downto 16)
    val addrModified = busAddr & U"xfff0ffff".resized
    val calcedPartAddr = partAddr.mux(
      0 -> U("x5"),
      1 -> U("x6"),
      2 -> U("x7"),
      3 -> U("x8"),
      4 -> U("x9"),
      5 -> U("xa"),
      6 -> U("xb"),
      7 -> U("xc"),
      8 -> U("xd"),
      9 -> U("xe"),
      10 -> U("xf"),
      default -> U("x0")
    )

    val addrMsbCleared = busAddr(busAddr.getWidth - 2 downto 0).resized

    val newAddr = UInt
    when(busAddr.msb) {
      newAddr := addrMsbCleared
    } otherwise {
      newAddr := addrModified | (calcedPartAddr << 16).resized
    }

    /*
     * Address masking is required because it may not be aligned to word
     * address.
     */
    val maskedAddr = (newAddr & U"xfffffffc".resized).resized

    maskedAddr
  }

  val spiCtrl = new SpiMasterCtrl()

  val ar = io.axi.readCmd
  val r = io.axi.readRsp

  val rValid = Reg(Bool) init (False)
  val rLast = Reg(Bool) init (False)
  val id = Reg(UInt(4 bits))
  val addr = Reg(UInt(spiCtrl.io.cmd.payload.addr.getWidth bits))
  val lenRead = Reg(UInt(spiCtrl.io.cmd.payload.lenRead.getWidth bits))
  val spiCmdReady = Reg(Bool)
  val ctBytes = Reg(UInt(log2Up(32 / g.widthData) bits))
  val data = Reg(Bits(32 bits))

  io.spi.sclk <> spiCtrl.io.spi.sclk
  io.spi.ss <> spiCtrl.io.spi.ss
  io.spi.mosi <> spiCtrl.io.spi.mosi
  spiCtrl.io.spi.miso <> io.spi.miso

  ar.ready := False
  spiCtrl.io.cmd.valid := False
  spiCtrl.io.cmd.payload.spiCmd := g.cmdReadArray
  spiCtrl.io.cmd.payload.withAddr := True

  val fsm = new StateMachine {
    val idle = new State with EntryPoint
    val sending = new State
    val reading = new State

    idle
      .whenIsActive {
        rValid := False
        rLast := False

        when(ar.valid) {
          val lenReadVal = (ar.payload.len + 1) * (32 / g.widthData)

          id := ar.payload.id
          addr := calcAddr(ar.payload.addr)
          lenRead := lenReadVal
          spiCmdReady := spiCtrl.io.cmd.ready

          spiCtrl.io.cmd.valid := True
          spiCtrl.io.cmd.payload.addr := calcAddr(ar.payload.addr)
          spiCtrl.io.cmd.payload.lenRead := lenReadVal
          ar.ready := True
          goto(sending)
        }
      }

    sending
      .whenIsActive {
        spiCmdReady := spiCtrl.io.cmd.ready

        spiCtrl.io.cmd.valid := !spiCmdReady

        when(spiCmdReady) {
          spiCmdReady := False
          ctBytes := 32 / g.widthData - 1
          goto(reading)
        }
      }

    reading
      .whenIsActive {
        when(spiCtrl.io.data.valid) {
          ctBytes := ctBytes - 1
          data := (data |>> 8) | (spiCtrl.io.data.payload.fragment << 24)

          when(ctBytes === 0) {
            ctBytes := 32 / g.widthData - 1
            rValid := True
            rLast := spiCtrl.io.data.last

            when(spiCtrl.io.data.last) {
              goto(idle)
            }
          }
        } otherwise {
          rValid := False
          rLast := False
        }
      }
  }

  val fsmResume = new StateMachine {
    val idle = new State with EntryPoint
    val sending = new State
    val done = new State

    idle
      .whenIsActive {
        when(True) {
          spiCmdReady := spiCtrl.io.cmd.ready

          spiCtrl.io.cmd.valid := True
          spiCtrl.io.cmd.payload.spiCmd := g.cmdResume
          spiCtrl.io.cmd.payload.withAddr := False
          spiCtrl.io.cmd.payload.lenRead := 0
          goto(sending)
        }
      }

    sending
      .whenIsActive {
        spiCtrl.io.cmd.valid := !spiCmdReady

        when(spiCtrl.io.data.valid) {
          goto(done)
        }
      }

    done
      .whenIsActive {
      }
  }

  spiCtrl.io.cmd.payload.addr := addr
  spiCtrl.io.cmd.payload.lenRead := lenRead
  r.valid := rValid
  r.payload.id := id
  r.payload.data := data
  r.payload.last := rLast
}
