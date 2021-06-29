package glec2

import spinal.core._
import spinal.lib._


case class RegFile(implicit val conf : GlecCoreParams) extends Component {
  val io = new Bundle {
    val rs1_addr = in UInt(conf.xlen bits)
    val rs1_data = out Bits(conf.xlen bits)
    val rs2_addr = in UInt(conf.xlen bits)
    val rs2_data = out Bits(conf.xlen bits)
    val dm_addr = in UInt(conf.xlen bits)
    val dm_data = out UInt(conf.xlen bits)
  }
}




