package glec2

import spinal.core._
import spinal.lib._

// the soc level
class GlecTopLevel extends Component {
  val io = new Bundle {
  }
}

object GlecTopLevel {
  def main(args: Array[String]) {
    implicit val glecCoreParams = GlecCoreParams(32, false);
    SpinalVerilog(new RegFile())
  }
}
