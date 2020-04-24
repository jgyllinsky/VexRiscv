package flogics.vexriscv.tinyfpga

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.simple.PipelinedMemoryBus
import spinal.lib.com.jtag.Jtag
import spinal.lib.com.spi.ddr.SpiXdrMaster
import spinal.lib.com.uart._
import spinal.lib.io.{InOutWrapper, TriStateArray}
import spinal.lib.misc.{InterruptCtrl, Prescaler, Timer}
import spinal.lib.soc.pinsec.{PinsecTimerCtrl, PinsecTimerCtrlExternal}
import vexriscv.plugin._
import vexriscv.{VexRiscv, VexRiscvConfig, plugin}
import spinal.lib.com.spi.ddr._
import spinal.lib.bus.simple._
import scala.collection.mutable.ArrayBuffer
import flogics.lib.pwm._
import flogics.lib.spi._
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


case class MyMurax(config : MuraxConfig) extends Component{
  import config._

  val io = new Bundle {
    //Clocks / reset
    val asyncReset = in Bool
    val mainClk = in Bool

    //Main components IO
    val jtag = slave(Jtag())

    //Peripherals IO
    val gpioA = master(TriStateArray(gpioWidth bits))
    val uart = master(Uart())
    val pwm = out Bool
    val spi = slave(Spi())

    val xip = ifGen(genXip)(master(SpiXdrMaster(xipConfig.ctrl.spi)))
  }


  val resetCtrlClockDomain = ClockDomain(
    clock = io.mainClk,
    config = ClockDomainConfig(
      resetKind = BOOT
    )
  )

  val resetCtrl = new ClockingArea(resetCtrlClockDomain) {
    val mainClkResetUnbuffered  = False

    //Implement an counter to keep the reset axiResetOrder high 64 cycles
    // Also this counter will automatically do a reset when the system boot.
    val systemClkResetCounter = Reg(UInt(6 bits)) init(0)
    when(systemClkResetCounter =/= U(systemClkResetCounter.range -> true)){
      systemClkResetCounter := systemClkResetCounter + 1
      mainClkResetUnbuffered := True
    }
    when(BufferCC(io.asyncReset)){
      systemClkResetCounter := 0
    }

    //Create all reset used later in the design
    val mainClkReset = RegNext(mainClkResetUnbuffered)
    val systemReset  = RegNext(mainClkResetUnbuffered)
  }


  val systemClockDomain = ClockDomain(
    clock = io.mainClk,
    reset = resetCtrl.systemReset,
    frequency = FixedFrequency(coreFrequency)
  )

  val debugClockDomain = ClockDomain(
    clock = io.mainClk,
    reset = resetCtrl.mainClkReset,
    frequency = FixedFrequency(coreFrequency)
  )

  val system = new ClockingArea(systemClockDomain) {
    val pipelinedMemoryBusConfig = PipelinedMemoryBusConfig(
      addressWidth = 32,
      dataWidth = 32
    )

    //Arbiter of the cpu dBus/iBus to drive the mainBus
    //Priority to dBus, !! cmd transactions can change on the fly !!
    val mainBusArbiter = new MuraxMasterArbiter(pipelinedMemoryBusConfig)

    //Instanciate the CPU
    val cpu = new VexRiscv(
      config = VexRiscvConfig(
        plugins = cpuPlugins += new DebugPlugin(debugClockDomain, hardwareBreakpointCount)
      )
    )

    //Checkout plugins used to instanciate the CPU to connect them to the SoC
    val timerInterrupt = False
    val externalInterrupt = False
    for(plugin <- cpu.plugins) plugin match{
      case plugin : IBusSimplePlugin =>
        mainBusArbiter.io.iBus.cmd <> plugin.iBus.cmd
        mainBusArbiter.io.iBus.rsp <> plugin.iBus.rsp
      case plugin : DBusSimplePlugin => {
        if(!pipelineDBus)
          mainBusArbiter.io.dBus <> plugin.dBus
        else {
          mainBusArbiter.io.dBus.cmd << plugin.dBus.cmd.halfPipe()
          mainBusArbiter.io.dBus.rsp <> plugin.dBus.rsp
        }
      }
      case plugin : CsrPlugin        => {
        plugin.externalInterrupt := externalInterrupt
        plugin.timerInterrupt := timerInterrupt
      }
      case plugin : DebugPlugin         => plugin.debugClockDomain{
        resetCtrl.systemReset setWhen(RegNext(plugin.io.resetOut))
        io.jtag <> plugin.io.bus.fromJtag()
      }
      case _ =>
    }



    //****** MainBus slaves ********
    val mainBusMapping = ArrayBuffer[(PipelinedMemoryBus,SizeMapping)]()
    val ram = new MuraxPipelinedMemoryBusRam(
      onChipRamSize = onChipRamSize,
      onChipRamHexFile = onChipRamHexFile,
      pipelinedMemoryBusConfig = pipelinedMemoryBusConfig
    )
    mainBusMapping += ram.io.bus -> (0x80000000l, onChipRamSize)

    val apbBridge = new PipelinedMemoryBusToApbBridge(
      apb3Config = Apb3Config(
        addressWidth = 20,
        dataWidth = 32
      ),
      pipelineBridge = pipelineApbBridge,
      pipelinedMemoryBusConfig = pipelinedMemoryBusConfig
    )
    mainBusMapping += apbBridge.io.pipelinedMemoryBus -> (0xF0000000l, 1 MB)



    //******** APB peripherals *********
    val apbMapping = ArrayBuffer[(Apb3, SizeMapping)]()
    val gpioACtrl = Apb3Gpio(gpioWidth = gpioWidth, withReadSync = true)
    io.gpioA <> gpioACtrl.io.gpio
    apbMapping += gpioACtrl.io.apb -> (0x00000, 4 kB)

    val uartCtrl = Apb3UartCtrl(uartCtrlConfig)
    uartCtrl.io.uart <> io.uart
    externalInterrupt setWhen(uartCtrl.io.interrupt)
    apbMapping += uartCtrl.io.apb  -> (0x10000, 4 kB)

    val pwmCtrl = new Apb3PwmCtrl(size = 8)
    pwmCtrl.io.output <> io.pwm
    apbMapping += pwmCtrl.io.apb  -> (0x11000, 4 kB)

    val spiCtrl = new Apb3SpiSlave(width = 16)
    spiCtrl.io.spi <> io.spi
    externalInterrupt setWhen(spiCtrl.io.interrupt)
    apbMapping += spiCtrl.io.apb  -> (0x12000, 4 kB)

    val timer = new MuraxApb3Timer()
    timerInterrupt setWhen(timer.io.interrupt)
    apbMapping += timer.io.apb     -> (0x20000, 4 kB)

    val xip = ifGen(genXip)(new Area{
      val ctrl = Apb3SpiXdrMasterCtrl(xipConfig)
      ctrl.io.spi <> io.xip
      externalInterrupt setWhen(ctrl.io.interrupt)
      apbMapping += ctrl.io.apb     -> (0x1F000, 4 kB)

      val accessBus = new PipelinedMemoryBus(PipelinedMemoryBusConfig(24,32))
      mainBusMapping += accessBus -> (0xE0000000l, 16 MB)

      ctrl.io.xip.fromPipelinedMemoryBus() << accessBus
      val bootloader = Apb3Rom("src/main/c/murax/xipBootloader/crt.bin")
      apbMapping += bootloader.io.apb     -> (0x1E000, 4 kB)
    })



    //******** Memory mappings *********
    val apbDecoder = Apb3Decoder(
      master = apbBridge.io.apb,
      slaves = apbMapping
    )

    val mainBusDecoder = new Area {
      val logic = new MuraxPipelinedMemoryBusDecoder(
        master = mainBusArbiter.io.masterBus,
        specification = mainBusMapping,
        pipelineMaster = pipelineMainBus
      )
    }
  }
}

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
      val pwm      = out Bool()
      val spi_sck  = in  Bool()
      val spi_mosi = in  Bool()
      val spi_ss   = in  Bool()
      val USBPU    = out Bool()

/*
      val mosi = inout(Analog(Bool))
      val miso = inout(Analog(Bool))
      val sclk = out Bool()
      val spis = out Bool()
*/

      val led = out Bits(8 bits)
    }
    val murax = MyMurax(
      MuraxConfig
        .default(withXip = false)
        .copy(
          coreFrequency = 16 MHz,
          onChipRamSize = 8 kB,
          onChipRamHexFile = "src/main/ressource/hex/pwmspiDemo.hex"
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
    murax.io.pwm <> io.pwm
    murax.io.spi.sck := io.spi_sck
    murax.io.spi.mosi := io.spi_mosi
    murax.io.spi.ss := io.spi_ss

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
