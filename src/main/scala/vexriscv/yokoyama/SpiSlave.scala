package flogics.lib.spi

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3SlaveFactory}

case class Spi() extends Bundle with IMasterSlave {
  val sck = Bool
  val ss = Bool
  val mosi = Bool
  val miso = Bool

  override def asMaster(): Unit = {
    out(sck)
    out(ss)
    out(mosi)
    in(miso)
  }
}

class SpiSlave(
    width: Int,
    reset_wait: Int = 3
) extends Component {
  val io = new Bundle {
    val spi = slave(Spi())
    val output = master(Flow(Bits(width bits)))
    val dummy_clk = in Bool
  }

  io.spi.miso := False

  /*
   * For simulation purpose
   */
  val dummy_sim_clock_domain = ClockDomain(
    clock = io.dummy_clk
  )

  /*
   * Create reset input for spi_clock_domain.
   * Required to clear the internal registers when spi.ss is high when started.
   */
  val ct_reset = Counter(reset_wait)
  when(!ct_reset.willOverflowIfInc) {
    ct_reset.increment()
  }

  val spi_reset = ct_reset.willOverflowIfInc && io.spi.ss
  val spi_clock_domain = ClockDomain(
    clock = io.spi.sck,
    reset = spi_reset,
    config = ClockDomainConfig(
      clockEdge = FALLING,
      resetKind = ASYNC,
      resetActiveLevel = HIGH
    )
  )

  val spi_clock_area = new ClockingArea(spi_clock_domain) {
    val ct_bits = Counter(width)
    val shift_reg = Reg(Bits(width bits)) init (0)
    val output = Reg(Bits(width bits))
    // We can't add "init(False)" to ct_full.
    // Otherwise, spi.ss clears ct_full unintentionally.
    val ct_full = Reg(Bool)

    ct_bits.increment()

    // ct_full won't be reset until next spi.sck edge comes
    val adjust: Int = 2
    when(ct_bits.willOverflow) {
      ct_full := True
    } elsewhen(ct_bits.value >= width / 2 - adjust) {
      ct_full := False
    }

    val nextval = B(
      width bits,
      (width - 1 downto 1) -> shift_reg(width - 2 downto 0),
      0 -> io.spi.mosi
    )

    when(ct_bits.willOverflow) {
      output := nextval
    } otherwise {
      shift_reg := nextval
    }
  }

  /*
   * started is required to gate ct_full at the empty state below
   */
  val started = Reg(Bool).addTag(crossClockDomain) init (False)
  when(!io.spi.ss) {
    started := True
  }

  /*
   * Refer
   *   https://spinalhdl.github.io/SpinalDoc-RTD/SpinalHDL/Structuring/clock_domain.html
   *   https://spinalhdl.github.io/SpinalDoc-RTD/SpinalHDL/Design%20errors/clock_crossing_violation.html
   */

  /*
   * spi_clock_area.ct_full may be latched earlier, so additional bufferDepth
   * for ct_full_cc.
   */
  val ct_full_cc = BufferCC(spi_clock_area.ct_full, bufferDepth = 3)
  val output_cc = BufferCC(spi_clock_area.output)

  val fsm = new StateMachine {
    val empty = new State with EntryPoint
    val waiting = new State
    val ready = new State
    val waiting_next = new State

    io.output.valid := False
    io.output.payload := B(0)

    empty
      .whenIsActive {
        when(started && ct_full_cc) {
          goto(ready)
        }
      }

    ready
      .whenIsActive {
        io.output.valid := True
        io.output.payload := output_cc
        goto(waiting_next)
      }

    waiting_next
      .whenIsActive {
        when(!ct_full_cc) {
          goto(empty)
        }
      }
  }
}

class Apb3SpiSlave(width: Int, fifoDepth: Int = 8) extends Component {
  val io = new Bundle {
    val apb = slave(
      Apb3(
        addressWidth = 4,
        dataWidth = 32
      )
    )
    val spi = slave(Spi())
    val interrupt = out Bool
  }

  val spislave = new SpiSlave(width)
  io.spi <> spislave.io.spi
  spislave.io.dummy_clk := False

  val busCtrl = Apb3SlaveFactory(io.apb)
  val q = spislave.io.output.toStream.queue(fifoDepth)
  busCtrl.readStreamNonBlocking(
    q,
    address = 0,
    validBitOffset = 31,
    payloadBitOffset = 0
  )
  busCtrl.read(q.valid, address = 4, bitOffset = 0)
  io.interrupt := q.valid
}

object SpiSlaveVerilog {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new SpiSlave(width = 16))
  }
}

import spinal.core._
import spinal.sim._
import spinal.core.sim._

object SpiSlaveSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.doSim(
      new SpiSlave(width = 16)
    ) { dut =>
      val sim_clkdom = ClockDomain(dut.io.dummy_clk)

      def wait_core(
          count: Int = 1,
          assertion: Boolean = true,
          msg: String = "wait_core"
      ) {
        for (i <- 0 until count) {
          dut.clockDomain.waitSampling(1)
          assert(assertion, msg)
        }
      }

      def wait_sim(count: Int = 1) {
        sim_clkdom.waitSampling(count)
      }

      def spi_mosi_send(v: Int) {
        wait_sim()
        dut.io.spi.ss #= false
        wait_sim()
        for (bit <- 15 to 0 by -1) {
          dut.io.spi.sck #= true
          dut.io.spi.mosi #= ((v >> bit) & 1) != 0
          wait_sim()
          dut.io.spi.sck #= false
          wait_sim()
        }
        wait_sim()
        dut.io.spi.ss #= true
      }

      sim_clkdom.forkStimulus(3)
      dut.clockDomain.forkStimulus(period = 10)

      dut.io.spi.sck #= false
      dut.io.spi.ss #= true
      dut.io.spi.mosi #= false

      // println(dut.io.output.valid.toBoolean)
      wait_core(10, dut.io.output.valid.toBoolean == false)

      spi_mosi_send(0x8765)
      for (i <- 0 until 5) {
        wait_core(1)
        if (dut.io.output.valid.toBoolean) {
          println(dut.io.output.payload)
          assert(dut.io.output.payload.toBigInt == 0x8765, "0x8765")
        }
      }

      wait_sim()
      dut.io.spi.ss #= true

      wait_sim(20)
      spi_mosi_send(0x5555)
      for (i <- 0 until 5) {
        wait_core(1)
        if (dut.io.output.valid.toBoolean)
          assert(dut.io.output.payload.toBigInt == 0x5555, "0x5555")
      }

      spi_mosi_send(0xaaaa)
      spi_mosi_send(0x8888)

      wait_core(10)
    }
  }
}
