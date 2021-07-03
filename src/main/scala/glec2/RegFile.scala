package glec2

import spinal.core._
import spinal.lib._

case class RegFileIO(implicit val conf: CoreParams) extends Bundle {
  val rs1_addr = in UInt(5 bits)
  val rs1_data = out Bits(conf.xlen bits)

  val rs2_addr = in UInt(5 bits)
  val rs2_data = out Bits(conf.xlen bits)

  val dm_addr = in UInt(5 bits)
  val dm_rdata = out Bits(conf.xlen bits)
  val dm_wdata = in Bits(conf.xlen bits)
  val dm_wen = in Bool()

  val waddr = in UInt(5 bits)
  val wdata = in Bits(conf.xlen bits)
  val wen = in Bool()
}

case class RegFile(implicit val conf : CoreParams) extends Component {
  val io = RegFileIO()

  val regFile = Mem(Bits(conf.xlen bits), 32) randBoot()

  when(io.wen && (io.waddr =/= U"0")) {
    regFile(io.waddr) := io.wdata
  };
  
  when(io.dm_wen && (io.dm_addr =/= U"0")) {
    regFile(io.dm_addr) := io.dm_wdata
  }

  io.rs1_data := Mux(io.rs1_addr =/= U"0", regFile(io.rs1_addr), B(0, conf.xlen bits));
  io.rs2_data := Mux(io.rs2_addr =/= U"0", regFile(io.rs2_addr), B(0, conf.xlen bits));
  io.dm_rdata := Mux(io.dm_addr =/= U"0", regFile(io.dm_addr), B(0, conf.xlen bits));
}




