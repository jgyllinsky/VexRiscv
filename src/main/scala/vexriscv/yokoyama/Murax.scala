package flogics.vexriscv.tinyfpga

import spinal.core._
import spinal.lib._
import vexriscv.demo._

/**
 * Created by PIC32F_USER on 28/07/2017.
 *
 * Modified by Yokoyama (Firmlogics) on 31/03/2020, for TinyFPGA-BX.
 * Currently, XIP (Execution-In-Place) on SPI ROM is not supported.
 *
 * (Following information is original instead of TinyFPGA-BX.)
 *
 * Murax is a very light SoC which could work without any external component.
 * - ICE40-hx8k + icestorm =>  53 Mhz, 2142 LC
 * - 0.37 DMIPS/Mhz
 * - 8 kB of on-chip ram
 * - JTAG debugger (eclipse/GDB/openocd ready)
 * - Interrupt support
 * - APB bus for peripherals
 * - 32 GPIO pin
 * - one 16 bits prescaler, two 16 bits timers
 * - one UART with tx/rx fifo
 */


object Murax_iCE40_tinyfpga_bx{

  case class SB_GB() extends BlackBox{
    val USER_SIGNAL_TO_GLOBAL_BUFFER = in Bool()
    val GLOBAL_BUFFER_OUTPUT = out Bool()
  }

  case class SB_IO_SCLK() extends BlackBox{
    addGeneric("PIN_TYPE", B"010000")
    val PACKAGE_PIN = out Bool()
    val OUTPUT_CLK = in Bool()
    val CLOCK_ENABLE = in Bool()
    val D_OUT_0 = in Bool()
    val D_OUT_1 = in Bool()
    setDefinitionName("SB_IO")
  }

  case class SB_IO_DATA() extends BlackBox{
    addGeneric("PIN_TYPE", B"110000")
    val PACKAGE_PIN = inout(Analog(Bool))
    val CLOCK_ENABLE = in Bool()
    val INPUT_CLK = in Bool()
    val OUTPUT_CLK = in Bool()
    val OUTPUT_ENABLE = in Bool()
    val D_OUT_0 = in Bool()
    val D_OUT_1 = in Bool()
    val D_IN_0 = out Bool()
    val D_IN_1 = out Bool()
    setDefinitionName("SB_IO")
  }

  case class Murax_iCE40_tinyfpga_bx() extends Component{
    val io = new Bundle {
      val mainClk  = in  Bool()
      val jtag_tck = in  Bool()
      val jtag_tdi = in  Bool()
      val jtag_tdo = out Bool()
      val jtag_tms = in  Bool()
      val uart_txd = out Bool()
      val uart_rxd = in  Bool()
      val USBPU    = out Bool()

/*
      val mosi = inout(Analog(Bool))
      val miso = inout(Analog(Bool))
      val sclk = out Bool()
      val spis = out Bool()
*/

      val led = out Bits(8 bits)
    }
    val murax = Murax(
      MuraxConfig
        .default(withXip = false)
        .copy(
          coreFrequency = 16 MHz,
          onChipRamSize = 8 kB,
          onChipRamHexFile = "src/main/ressource/hex/muraxDemo.hex"
        )
    )
    murax.io.asyncReset := False

    val mainClkBuffer = SB_GB()
    mainClkBuffer.USER_SIGNAL_TO_GLOBAL_BUFFER <> io.mainClk
    mainClkBuffer.GLOBAL_BUFFER_OUTPUT <> murax.io.mainClk

    val jtagClkBuffer = SB_GB()
    jtagClkBuffer.USER_SIGNAL_TO_GLOBAL_BUFFER <> io.jtag_tck
    jtagClkBuffer.GLOBAL_BUFFER_OUTPUT <> murax.io.jtag.tck

    io.led <> murax.io.gpioA.write(7 downto 0)

    murax.io.jtag.tdi <> io.jtag_tdi
    murax.io.jtag.tdo <> io.jtag_tdo
    murax.io.jtag.tms <> io.jtag_tms
    murax.io.gpioA.read <> 0
    murax.io.uart.txd <> io.uart_txd
    murax.io.uart.rxd <> io.uart_rxd

    /*
     * Please refer
     * https://github.com/tinyfpga/TinyFPGA-BX/blob/master/apio_template/top.v
     *
     * Drive USB pull-up resistor to 'low' to disable USB
     */
    io.USBPU := False


/*
    val xip = new ClockingArea(murax.systemClockDomain) {
      RegNext(murax.io.xip.ss.asBool) <> io.spis

      val sclkIo = SB_IO_SCLK()
      sclkIo.PACKAGE_PIN <> io.sclk
      sclkIo.CLOCK_ENABLE := True

      sclkIo.OUTPUT_CLK := ClockDomain.current.readClockWire
      sclkIo.D_OUT_0 <> murax.io.xip.sclk.write(0)
      sclkIo.D_OUT_1 <> RegNext(murax.io.xip.sclk.write(1))

      val datas = for ((data, pin) <- (murax.io.xip.data, List(io.mosi, io.miso)).zipped) yield new Area {
        val dataIo = SB_IO_DATA()
        dataIo.PACKAGE_PIN := pin
        dataIo.CLOCK_ENABLE := True

        dataIo.OUTPUT_CLK := ClockDomain.current.readClockWire
        dataIo.OUTPUT_ENABLE <> data.writeEnable
        dataIo.D_OUT_0 <> data.write(0)
        dataIo.D_OUT_1 <> RegNext(data.write(1))

        dataIo.INPUT_CLK := ClockDomain.current.readClockWire
        data.read(0) := dataIo.D_IN_0
        data.read(1) := RegNext(dataIo.D_IN_1)
      }
    }
*/

  }

  def main(args: Array[String]) {
    SpinalVerilog(Murax_iCE40_tinyfpga_bx())
  }
}
