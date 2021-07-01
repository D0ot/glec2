package glec2

import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.core.sim.SpinalSimConfig
import spinal.core.sim.SimClockDomainPimper

// the soc level
class GlecTopLevel extends Component {
  val io = new Bundle {
  }
}

object GlecTopLevel {
  def main(args: Array[String]) {
    implicit val glecCoreParams = CoreParams(32, false);
    val config = SpinalConfig(defaultClockDomainFrequency = FixedFrequency(1 MHz))

    val simConfig = SpinalSimConfig().withConfig(config)
      .withWave
      .compile(new GlecCore)
      .doSim { dut =>
        dut.clockDomain.get.forkStimulus(100)
        for(i <- 0 until 100) {
          dut.clockDomain.get.waitFallingEdge()
        }
      }
  }
}
