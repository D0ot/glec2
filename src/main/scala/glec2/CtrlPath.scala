package glec2

import spinal.core._
import spinal.lib._


case class ICacheBus(implicit conf : CoreParams) extends Bundle with IMasterSlave {
  val pc = UInt(conf.pcWidth bits)
  val ins = Bits(conf.xlen bits)
  def asMaster(): Unit = {
    out (pc)
    in (ins)
  }
}


case class Ctrl2DataIO(implicit conf : CoreParams) extends Bundle with IMasterSlave{

  // ID stage
  val rs1 = UInt(5 bits)
  val rs2 = UInt(5 bits)
  val imm = Bits(32 bits)
  
  // EXE stage
  val alu_op1_sel = ALUOp1Sel()
  val alu_op2_sel = ALUOp2Sel()
  val alu_opcode = ALUOpcode()
  val ins_bit30 = Bool()
  val is_branch = Bool()
  val exe_pc = UInt(conf.pcWidth bits)

  // Mem stage
  val wen = Bool()
  val store_type = StoreType()

  // WB stage
  val rd = UInt(5 bits)
  val reg_wen = Bool()
  val wb_sel = WriteBackSel()
  val load_type = LoadType()

  def asMaster(): Unit = {
    out(rs1)
    out(rs2)
    out(imm)

    out(alu_op1_sel)
    out(alu_op2_sel)
    out(alu_opcode)
    out(is_branch)

    out(wen)
    out(store_type)

    out(rd)
    out(reg_wen)
    out(wb_sel)
    out(load_type)
  }
}

// fetch decode execute memory writeback
// fet, dec, exe, mem, wb
case class CtrlPath(implicit conf : CoreParams) extends Component {
  val io = new Bundle {
    val icb = master (ICacheBus())
    val c2d = master (Ctrl2DataIO())
    val d2c = slave (Data2CtrlIO())
  }
  val pc = Reg(UInt(conf.pcWidth bits)) init(conf.pcInitVal)

  val dec_ir = Reg(Bits(conf.xlen bits)) 
  val dec_pc = RegNext(pc)

  io.icb.pc := pc
  dec_ir := io.icb.ins

  val dec_ic = InstructionCtrl(conf, dec_ir, dec_pc)

  // decode stage
  io.c2d.rs1 := dec_ic.rs1
  io.c2d.rs2 := dec_ic.rs2
  io.c2d.imm := dec_ic.imm
}
























