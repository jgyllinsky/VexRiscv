package flogics.lib.axi4

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3SlaveFactory}
import spinal.lib.bus.amba4.axi.{Axi4Config, Axi4WriteOnly}

class SimpleAxi4Master(
    addr_begin: Long = 0x80000f00L,
    bits_range: Int = 8
) extends Component {
  val io = new Bundle {
    val axi = master(
      Axi4WriteOnly(
        Axi4Config(
          addressWidth = 32,
          dataWidth = 32,
          useId = false,
          useRegion = false,
          useBurst = false,
          useLock = false,
          useCache = false,
          useSize = false,
          useQos = false,
          useLen = false,
          // 'false' useLast causes an assertion error in Axi4WriteOnlyDecoder
          useLast = true,
          useResp = false,
          useProt = false,
          useStrb = false
        )
      )
    )
  }

  SpinalInfo("addr_begin = " + addr_begin.toString)

  val addr = Reg(UInt(bits_range bits)) init (0)
  val data = Counter(32 bits)
  val ct = Counter(1000)

  io.axi.writeCmd.valid := False
  io.axi.writeCmd.payload.addr := U(0, 32 bits)
  io.axi.writeData.valid := False
  io.axi.writeData.last := True
  io.axi.writeData.payload.data := B(0, 32 bits)
  io.axi.writeRsp.ready := True

  ct.increment()

  val fsmAddr = new StateMachine {
    val idle = new State with EntryPoint
    val writing = new State

    idle
      .whenIsActive {
        when(ct.willOverflow) {
          ct.clear()
          goto(writing)
        }
      }

    writing
      .whenIsActive {
        io.axi.writeCmd.valid := True
        io.axi.writeCmd.payload.addr := addr_begin | addr.resized
        when(io.axi.writeCmd.ready) {
          when(addr === (1 << bits_range) - 4) {
            addr := 0
          } otherwise {
            addr := addr + 4
          }
          goto(idle)
        }
      }
  }

  val fsmData = new StateMachine {
    val idle = new State with EntryPoint
    val writing = new State

    idle
      .whenIsActive {
        when(ct.willOverflow) {
          goto(writing)
        }
      }

    writing
      .whenIsActive {
        io.axi.writeData.valid := True
        io.axi.writeData.payload.data := B(data.value)
        when(io.axi.writeData.ready) {
          data.increment()
          goto(idle)
        }
      }
  }
}
