package flogics.vexriscv.tinyfpga


import vexriscv.plugin._
import vexriscv._
import vexriscv.ip.{DataCacheConfig, InstructionCacheConfig}
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba4.axi._
import spinal.lib.com.jtag.Jtag
import spinal.lib.com.uart._
import spinal.lib.graphic.RgbConfig
import spinal.lib.graphic.vga.{Axi4VgaCtrl, Axi4VgaCtrlGenerics, Vga}
import spinal.lib.io.TriStateArray
import spinal.lib.memory.sdram.SdramGeneration.SDR
import spinal.lib.memory.sdram._
import spinal.lib.memory.sdram.sdr.{Axi4SharedSdramCtrl, IS42x320D, SdramInterface, SdramTimings}
import spinal.lib.misc.HexTools
import spinal.lib.soc.pinsec.{PinsecTimerCtrl, PinsecTimerCtrlExternal}
import spinal.lib.system.debugger.{JtagAxi4SharedDebugger, JtagBridge, SystemDebugger, SystemDebuggerConfig}
import spinal.lib.blackbox.lattice.ice40.SB_IO
import flogics.lib.pwm._
import flogics.lib.spi._
import flogics.lib.axi4._

import scala.collection.mutable.ArrayBuffer
import spinal.lib.com.spi.SpiMaster
import flogics.lib.axi4.Axi4RomAt25sf081


case class BrieyConfig(axiFrequency : HertzNumber,
                       onChipRamSize : BigInt,
                       sdramLayout: SdramLayout,
                       sdramTimings: SdramTimings,
                       cpuPlugins : ArrayBuffer[Plugin[VexRiscv]],
                       uartCtrlConfig : UartCtrlMemoryMappedConfig)

object BrieyConfig{

  def default = {
    val config = BrieyConfig(
      axiFrequency = 50 MHz,
      onChipRamSize  = 4 kB,
      sdramLayout = IS42x320D.layout,
      sdramTimings = IS42x320D.timingGrade7,
      uartCtrlConfig = UartCtrlMemoryMappedConfig(
        uartCtrlConfig = UartCtrlGenerics(
          dataWidthMax      = 8,
          clockDividerWidth = 20,
          preSamplingSize   = 1,
          samplingSize      = 5,
          postSamplingSize  = 2
        ),
        txFifoDepth = 16,
        rxFifoDepth = 16
      ),
      cpuPlugins = ArrayBuffer(
        new PcManagerSimplePlugin(0x80000000l, false),
        //          new IBusSimplePlugin(
        //            interfaceKeepData = false,
        //            catchAccessFault = true
        //          ),
        new IBusCachedPlugin(
          resetVector = 0x80000000l,
          prediction = STATIC,
          config = InstructionCacheConfig(
            cacheSize = 4096,
            bytePerLine =32,
            wayCount = 1,
            addressWidth = 32,
            cpuDataWidth = 32,
            memDataWidth = 32,
            catchIllegalAccess = true,
            catchAccessFault = true,
            asyncTagMemory = false,
            twoCycleRam = true,
            twoCycleCache = true
          )
          //            askMemoryTranslation = true,
          //            memoryTranslatorPortConfig = MemoryTranslatorPortConfig(
          //              portTlbSize = 4
          //            )
        ),
        //                    new DBusSimplePlugin(
        //                      catchAddressMisaligned = true,
        //                      catchAccessFault = true
        //                    ),
        new DBusCachedPlugin(
          config = new DataCacheConfig(
            cacheSize         = 4096,
            bytePerLine       = 32,
            wayCount          = 1,
            addressWidth      = 32,
            cpuDataWidth      = 32,
            memDataWidth      = 32,
            catchAccessError  = true,
            catchIllegal      = true,
            catchUnaligned    = true
          ),
          memoryTranslatorPortConfig = null
          //            memoryTranslatorPortConfig = MemoryTranslatorPortConfig(
          //              portTlbSize = 6
          //            )
        ),
        new StaticMemoryTranslatorPlugin(
          ioRange      = _(31 downto 28) === 0xF
        ),
        new DecoderSimplePlugin(
          catchIllegalInstruction = true
        ),
        new RegFilePlugin(
          regFileReadyKind = plugin.SYNC,
          zeroBoot = false
        ),
        new IntAluPlugin,
        new SrcPlugin(
          separatedAddSub = false,
          executeInsertion = true
        ),
        new FullBarrelShifterPlugin,
        new MulPlugin,
        new DivPlugin,
        new HazardSimplePlugin(
          bypassExecute           = true,
          bypassMemory            = true,
          bypassWriteBack         = true,
          bypassWriteBackBuffer   = true,
          pessimisticUseSrc       = false,
          pessimisticWriteRegFile = false,
          pessimisticAddressMatch = false
        ),
        new BranchPlugin(
          earlyBranch = false,
          catchAddressMisaligned = true
        ),
        new CsrPlugin(
          config = CsrPluginConfig(
            catchIllegalAccess = false,
            mvendorid      = null,
            marchid        = null,
            mimpid         = null,
            mhartid        = null,
            misaExtensionsInit = 66,
            misaAccess     = CsrAccess.NONE,
            mtvecAccess    = CsrAccess.NONE,
            mtvecInit      = 0x80000020l,
            mepcAccess     = CsrAccess.READ_WRITE,
            mscratchGen    = false,
            mcauseAccess   = CsrAccess.READ_ONLY,
            mbadaddrAccess = CsrAccess.READ_ONLY,
            mcycleAccess   = CsrAccess.NONE,
            minstretAccess = CsrAccess.NONE,
            ecallGen       = false,
            wfiGenAsWait         = false,
            ucycleAccess   = CsrAccess.NONE
          )
        ),
        new YamlPlugin("cpu0.yaml")
      )
    )
    config
  }

  def flogics(): BrieyConfig = flogics(withXip = false)
  def flogics(withXip: Boolean) =
    BrieyConfig(
      axiFrequency = 0 MHz, // dummy anymore
      onChipRamSize  = 8 kB,
      sdramLayout = null,
      sdramTimings = null,
      uartCtrlConfig = UartCtrlMemoryMappedConfig(
        uartCtrlConfig = UartCtrlGenerics(
          dataWidthMax      = 8,
          clockDividerWidth = 20,
          preSamplingSize   = 1,
          samplingSize      = 3,
          postSamplingSize  = 1
        ),
        initConfig = UartCtrlInitConfig(
          baudrate = 115200,
          dataLength = 7,  //7 => 8 bits
          parity = UartParityType.NONE,
          stop = UartStopType.ONE
        ),
        busCanWriteClockDividerConfig = false,
        busCanWriteFrameConfig = false,
        txFifoDepth = 16,
        rxFifoDepth = 16
      ),
      cpuPlugins = ArrayBuffer(
        new IBusSimplePlugin(
          resetVector = 0x80000000l,
          cmdForkOnSecondStage = true,
          // vexriscv.plugin.IBusSimpleBus.toAxi4ReadOnly requires below
          cmdForkPersistence = true,
          prediction = NONE,
          catchAccessFault = false,
          compressedGen = false
        ),
        new DBusSimplePlugin(
          catchAddressMisaligned = false,
          catchAccessFault = false,
          earlyInjection = false
        ),
        new CsrPlugin(
          CsrPluginConfig.smallest(
            mtvecInit = 0x80000020l
          )
        ),
        new DecoderSimplePlugin(
          catchIllegalInstruction = false
        ),
        new RegFilePlugin(
          regFileReadyKind = plugin.SYNC,
          zeroBoot = false
        ),
        new IntAluPlugin,
        new SrcPlugin(
          separatedAddSub = false,
          executeInsertion = false
        ),
        // Caution: LightShifterPlugin or FullBarrelShifterPlugin are
        //          mandatory.  If omitted, slli inst. works as add (adder).
        new LightShifterPlugin,
        new HazardSimplePlugin(
          bypassExecute = false,
          bypassMemory = false,
          bypassWriteBack = false,
          bypassWriteBackBuffer = false,
          pessimisticUseSrc = false,
          pessimisticWriteRegFile = false,
          pessimisticAddressMatch = false
        ),
        new BranchPlugin(
          earlyBranch = false,
          catchAddressMisaligned = false
        ),
        new YamlPlugin("cpu0.yaml")
      )
    )
}



class MiniBriey(
    config: BrieyConfig,
    axiClkFreq: HertzNumber,
    withXip: Boolean = false
) extends Component{

  //Legacy constructor
  def this(axiFrequency: HertzNumber) {
    this(
      BrieyConfig.default.copy(axiFrequency = axiFrequency),
      axiClkFreq = axiFrequency
    )
  }

  import config._
  val debug = true
  val interruptCount = 4
  def vgaRgbConfig = RgbConfig(5,6,5)

  val io = new Bundle{
    //Clocks / reset
    val asyncReset = in Bool
    val axiClk     = in Bool

    //Main components IO
    val jtag = slave(Jtag())

    //Peripherals IO
    val gpioA = master(TriStateArray(32 bits))
    val uart  = master(Uart())
    val pwm   = out Bool
    val spi   = slave(Spi())
    val spirom = master(SpiMaster())
  }

  val resetCtrlClockDomain = ClockDomain(
    clock = io.axiClk,
    config = ClockDomainConfig(
      resetKind = BOOT
    )
  )

  val resetCtrl = new ClockingArea(resetCtrlClockDomain) {
    /*
     * We need to enough time margin for SPI ROM
     * Reference: Adesto DS-25SF081–045I–8/2017
     *   11.3 Deep Power-Down (B9h)
     *   11.4 Resume from Deep Power-Down (ABh)
     */
    val timeWait = 10 us
    val marginBy = 2
    val cycleWait1 = (timeWait * axiClkFreq).toBigInt
    val cycleWait2 =
      (timeWait * 2 * axiClkFreq + (2 * 8 + 2) * marginBy).toBigInt

    SpinalInfo("timeWait = " + timeWait.toString)
    SpinalInfo("cycleWait1 = " + cycleWait1.toString)
    SpinalInfo("cycleWait2 = " + cycleWait2.toString)

    val reset1 = Reg(Bool) init (True)
    val reset2a = Reg(Bool) init (True)
    val reset2b = Reg(Bool) init (True)

    val systemResetCounter = Counter(cycleWait2 + 1)

    when(!systemResetCounter.willOverflowIfInc) {
      systemResetCounter.increment()
    }

    when(systemResetCounter.value === cycleWait1) {
      reset1 := False
    }

    when(systemResetCounter.value === cycleWait2) {
      reset2a := False
      reset2b := False
    }

    when(BufferCC(io.asyncReset)) {
      systemResetCounter.clear()
    }

    // Create all reset used later in the design
    // NOTICE: If generating systemReset and axiReset by single reset register,
    // gdb (by OpenOCD) fails to halt CPU.  (i.e. monitor reset halt)
    // I don't know why...  max_fanout problem?  (Should refer nextpnr output
    // log, etc.)
    val spiRomReset  = reset1
    val systemReset  = reset2a
    val axiReset     = reset2b
  }

  val spiRomClockDomain = ClockDomain(
    clock = io.axiClk,
    reset = resetCtrl.spiRomReset,
    frequency = FixedFrequency(axiClkFreq)
  )

  val spiRomArea = new ClockingArea(spiRomClockDomain) {
    /*
     * SPI ROM of TinyFPGA BX is 1 MB, but allow different access methods
     * in twice address range.
     */
    val spiRom = new Axi4RomAt25sf081(byteCount = 2 MB)
  }

  val axiClockDomain = ClockDomain(
    clock = io.axiClk,
    reset = resetCtrl.axiReset,
    frequency = FixedFrequency(axiClkFreq)
  )

  val debugClockDomain = ClockDomain(
    clock = io.axiClk,
    reset = resetCtrl.systemReset,
    frequency = FixedFrequency(axiClkFreq)
  )

  val externalInterrupt = False
  val axi = new ClockingArea(axiClockDomain) {
    val ram = Axi4SharedOnChipRam(
      dataWidth = 32,
      byteCount = onChipRamSize,
      idWidth = 4
    )

    val apbBridge = Axi4SharedToApb3Bridge(
      addressWidth = 20,
      dataWidth    = 32,
      idWidth      = 4
    )

    val gpioACtrl = Apb3Gpio(
      gpioWidth = 32,
      withReadSync = true
    )

    val timerCtrl = PinsecTimerCtrl()
    val timerCtrlExternal = PinsecTimerCtrlExternal()
    timerCtrl.io.external := timerCtrlExternal
    timerCtrlExternal.clear := False
    timerCtrlExternal.tick := False

    val uartCtrl = Apb3UartCtrl(uartCtrlConfig)
    externalInterrupt setWhen(uartCtrl.io.interrupt)
    SpinalInfo("clock = " + ClockDomain.current.frequency.getValue.toString)

    val pwmCtrl = new Apb3PwmCtrl(size = 8)
    pwmCtrl.io.output <> io.pwm

    val spiCtrl = new Apb3SpiSlave(width = 16)
    spiCtrl.io.spi <> io.spi
    externalInterrupt setWhen(spiCtrl.io.interrupt)

    val core = new Area{
      val config = VexRiscvConfig(
        plugins = cpuPlugins +=
          new DebugPlugin(
            debugClockDomain,
            hardwareBreakpointCount = if (withXip) 3 else 0)
      )

      val cpu = new VexRiscv(config)
      var iBus : Axi4ReadOnly = null
      var dBus : Axi4Shared = null
      for(plugin <- config.plugins) plugin match{
        case plugin : IBusSimplePlugin => iBus = plugin.iBus.toAxi4ReadOnly()
        case plugin : IBusCachedPlugin => iBus = plugin.iBus.toAxi4ReadOnly()
        case plugin : DBusSimplePlugin => dBus = plugin.dBus.toAxi4Shared()
        case plugin : DBusCachedPlugin => dBus = plugin.dBus.toAxi4Shared(true)
        case plugin : CsrPlugin        => {
          plugin.externalInterrupt := externalInterrupt
          plugin.timerInterrupt := timerCtrl.io.interrupt
        }
        case plugin : DebugPlugin      => debugClockDomain{
          resetCtrl.axiReset setWhen(RegNext(plugin.io.resetOut))
          io.jtag <> plugin.io.bus.fromJtag()
        }
        case _ =>
      }
    }

    val simpleAxi4Master = new SimpleAxi4Master()
    val spiRom = spiRomArea.spiRom
    spiRom.io.spi <> io.spirom


    val axiCrossbar = Axi4CrossbarFactory()

    SpinalInfo("withXip = " + withXip.toString)
    axiCrossbar.addSlaves(
      ram.io.axi -> (
        if (withXip)
          0x90000000L
        else
          0x80000000L, onChipRamSize),
      /*
       * SPI ROM of TinyFPGA BX is 1 MB, but allow different access methods
       * in twice address range.
       */
      spiRom.io.axi -> (
        if (withXip)
          0x80000000L
        else
          0x90000000L, 2 MB),
      apbBridge.io.axi -> (0xF0000000L,   1 MB)
    )

    axiCrossbar.addConnections(
      core.iBus       -> List(ram.io.axi, spiRom.io.axi),
      core.dBus       -> List(ram.io.axi, spiRom.io.axi, apbBridge.io.axi),
      simpleAxi4Master.io.axi -> List(ram.io.axi)
    )

    axiCrossbar.addPipelining(apbBridge.io.axi)((crossbar,bridge) => {
      crossbar.sharedCmd.halfPipe() >> bridge.sharedCmd
      crossbar.writeData.halfPipe() >> bridge.writeData
      crossbar.writeRsp             << bridge.writeRsp
      crossbar.readRsp              << bridge.readRsp
    })

    axiCrossbar.addPipelining(ram.io.axi)((crossbar,ctrl) => {
      crossbar.sharedCmd.halfPipe()  >>  ctrl.sharedCmd
      crossbar.writeData            >/-> ctrl.writeData
      crossbar.writeRsp              <<  ctrl.writeRsp
      crossbar.readRsp               <<  ctrl.readRsp
    })

    axiCrossbar.addPipelining(core.dBus)((cpu,crossbar) => {
      cpu.sharedCmd             >>  crossbar.sharedCmd
      cpu.writeData             >>  crossbar.writeData
      cpu.writeRsp              <<  crossbar.writeRsp
      cpu.readRsp               <-< crossbar.readRsp //Data cache directly use read responses without buffering, so pipeline it for FMax
    })

    axiCrossbar.build()


    val apbDecoder = Apb3Decoder(
      master = apbBridge.io.apb,
      slaves = List(
        gpioACtrl.io.apb -> (0x00000, 4 kB),
        uartCtrl.io.apb  -> (0x10000, 4 kB),
        pwmCtrl.io.apb   -> (0x11000, 4 kB),
        spiCtrl.io.apb   -> (0x12000, 4 kB),
        timerCtrl.io.apb -> (0x20000, 4 kB)
      )
    )
  }

  io.gpioA          <> axi.gpioACtrl.io.gpio
  io.uart           <> axi.uartCtrl.io.uart
}

object Briey_iCE40_tinyfpga_bx{

  case class SB_GB() extends BlackBox{
    val USER_SIGNAL_TO_GLOBAL_BUFFER = in Bool()
    val GLOBAL_BUFFER_OUTPUT = out Bool()
  }

  case class Briey_iCE40_tinyfpga_bx(
      nClkDiv: Int = 0,
      withXip: Boolean
  ) extends Component{
    val io = new Bundle {
      val mainClk = in Bool()
      val jtag_tck = in Bool()
      val jtag_tdi = in Bool()
      val jtag_tdo = out Bool()
      val jtag_tms = in Bool()
      val uart_txd = out Bool()
      val uart_rxd = in(Analog(Bool))
      val pwm = out Bool()
      val spi_sck = in Bool()
      val spi_mosi = in Bool()
      val spi_ss = in Bool()
      val spirom_sck = out Bool()
      val spirom_mosi = out Bool()
      val spirom_miso = in Bool()
      val spirom_ss = out Bool()
      val USBPU = out Bool()
      val led = out Bits(8 bits)
    }
    val briey = new MiniBriey(
      BrieyConfig.flogics,
      axiClkFreq = (16 / (1 << nClkDiv)) MHz,
      withXip = withXip
    )

    if (!withXip)
      HexTools.initRam(
        briey.axi.ram.ram,
        "src/main/ressource/hex/pwmSpiDemo.hex",
        0x80000000l
      )

    briey.io.asyncReset := False

    val dividedClk = Bool
    if (nClkDiv > 0) {
      val clkDivDomain = ClockDomain(
        clock = io.mainClk,
        config = ClockDomainConfig(
          resetKind = BOOT
        )
      )

      val clkDivArea = new ClockingArea(clkDivDomain) {
        val ct = Counter(nClkDiv bits)
        ct.increment()
        dividedClk := ct.willOverflow
      }
    }

    val mainClkBuffer = SB_GB()
    if (nClkDiv > 0)
       mainClkBuffer.USER_SIGNAL_TO_GLOBAL_BUFFER <> dividedClk
    else
      mainClkBuffer.USER_SIGNAL_TO_GLOBAL_BUFFER <> io.mainClk
    mainClkBuffer.GLOBAL_BUFFER_OUTPUT <> briey.io.axiClk

    val jtagClkBuffer = SB_GB()
    jtagClkBuffer.USER_SIGNAL_TO_GLOBAL_BUFFER <> io.jtag_tck
    jtagClkBuffer.GLOBAL_BUFFER_OUTPUT <> briey.io.jtag.tck

    io.led <> briey.io.gpioA.write(7 downto 0)

    briey.io.jtag.tdi <> io.jtag_tdi
    briey.io.jtag.tdo <> io.jtag_tdo
    briey.io.jtag.tms <> io.jtag_tms
    briey.io.gpioA.read <> 0
    briey.io.uart.txd <> io.uart_txd

    class SB_IO_PULLUP(pinType: String) extends SB_IO(pinType) {
      addGeneric("PULLUP", 1)
    }

    /*
     * Pull-up of rxd is required to avoid UART "break" condition.
     * Break condition will disable UART Rx and also Tx.
     * Refer
     * https://en.wikipedia.org/wiki/Universal_asynchronous_receiver-transmitter#Break_condition
     */
    val rxdPullup = new SB_IO_PULLUP(pinType = "000001")
    rxdPullup.PACKAGE_PIN := io.uart_rxd
    briey.io.uart.rxd := rxdPullup.D_IN_0

    briey.io.pwm <> io.pwm
    briey.io.spi.sck := io.spi_sck
    briey.io.spi.mosi := io.spi_mosi
    briey.io.spi.ss := io.spi_ss

    briey.io.spirom.sclk <> io.spirom_sck
    briey.io.spirom.mosi <> io.spirom_mosi
    briey.io.spirom.miso <> io.spirom_miso
    briey.io.spirom.ss.asBool <> io.spirom_ss

    /*
     * Please refer
     * https://github.com/tinyfpga/TinyFPGA-BX/blob/master/apio_template/top.v
     *
     * Drive USB pull-up resistor to 'low' to disable USB
     */
    io.USBPU := False
  }

  def main(args: Array[String]) {
    SpinalVerilog(Briey_iCE40_tinyfpga_bx(nClkDiv = 0, withXip = true))
  }
}

object Briey{
  def main(args: Array[String]) {
    val config = SpinalConfig()
    config.generateVerilog({
      val toplevel = new MiniBriey(
        BrieyConfig.flogics,
        axiClkFreq = 16 MHz,
        withXip = true
      )
      toplevel
    })
  }
}
