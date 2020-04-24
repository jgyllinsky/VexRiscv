package flogics.lib.pwm

import spinal.core._
import spinal.sim._
import spinal.core.sim._

package object mine {
  def my_assert(f: Boolean, msg: String): Unit = {
    assert(
      assertion = f,
      message = msg
    )
  }
}

import mine._

object Apb3PwmCtrlSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.doSim(
      new Apb3PwmCtrl(
        size = 8
      )
    ) { dut =>
      def wait(count: Int = 1) {
        dut.clockDomain.waitSampling(count)
      }

      def write(addr: BigInt, data: BigInt): Unit = {
        dut.io.apb.PADDR #= addr
        dut.io.apb.PSEL #= 1
        dut.io.apb.PENABLE #= false
        dut.io.apb.PWRITE #= true
        dut.io.apb.PWDATA #= data

        wait()
        dut.io.apb.PENABLE #= true

        wait()
        dut.io.apb.PSEL #= 0
        dut.io.apb.PENABLE #= false
      }

      def read_assert(addr: BigInt, expecting: BigInt, msg: String): Unit = {
        dut.io.apb.PADDR #= addr
        dut.io.apb.PSEL #= 1
        dut.io.apb.PENABLE #= false
        dut.io.apb.PWRITE #= false

        wait()
        dut.io.apb.PENABLE #= true

        wait()
        my_assert(
          dut.io.apb.PRDATA.toBigInt == expecting,
          msg + " expected PRDATA = " + expecting.toString
            + ", actual PRDATA = " + dut.io.apb.PRDATA.toBigInt.toString
        )

        dut.io.apb.PSEL #= 0
        dut.io.apb.PENABLE #= false
      }

      dut.clockDomain.forkStimulus(period = 10)

      val REG = 0x0

      /*
       * Initialize inputs
       */
      dut.io.apb.PADDR #= 0
      dut.io.apb.PSEL #= 0
      dut.io.apb.PENABLE #= false
      dut.io.apb.PWRITE #= false
      dut.io.apb.PWDATA #= 0

      wait(10)
      write(REG, 0x5a)

      wait(5)
      read_assert(REG, 0x5a, "reading")

      wait(300)
    }
  }
}
