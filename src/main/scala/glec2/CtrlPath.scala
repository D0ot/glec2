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
  val do_sub = Bool()

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
    out(ins_bit30)
    out(is_branch)
    out(exe_pc)
    out(do_sub)

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

  val flash = Bool()
  val dec_kill = Bool()
  val rest_done = RegNext(True) init(False)

  dec_kill := False 

  val pc = Reg(UInt(conf.pcWidth bits)) init(conf.pcInitVal)
  io.icb.pc := pc

  val pc_plus_4 = pc + U(4)

  // ID stage
  val dec_ir = Mux(dec_kill, Misc.NOP, io.icb.ins)
  val dec_pc = RegNext(pc) init(conf.pcInitVal)
  val dec_ic = InstructionCtrl(conf, dec_ir, dec_pc)


  io.c2d.rs1 := dec_ic.rs1
  io.c2d.rs2 := dec_ic.rs2
  io.c2d.imm := dec_ic.imm

  // EXE stage
  val exe_ir = Reg(Bits(conf.xlen bits)) init(Misc.NOP)
  exe_ir := dec_ir
  val exe_pc = RegNext(dec_pc) init(conf.pcInitVal)
  val exe_ic = Reg(new InstructionCtrl()) init(InstructionCtrl(conf, Misc.NOP, U(0)))
  exe_ic := dec_ic

  io.c2d.alu_op1_sel := exe_ic.alu_op1_sel
  io.c2d.alu_op2_sel := exe_ic.alu_op2_sel
  io.c2d.alu_opcode := exe_ic.alu_opcode
  io.c2d.ins_bit30 := exe_ic.ins_bit30
  io.c2d.is_branch := exe_ic.is_branch
  io.c2d.do_sub := exe_ic.alu_do_sub
  io.c2d.exe_pc := exe_pc

  // next pc calculation
  val alu_eq = io.d2c.alu_ret === B(0, conf.xlen bits)
  val alu_less = io.d2c.alu_ret(0)

  val should_br = exe_ic.bc.mux(
    BranchCond.EQ -> alu_eq,
    BranchCond.NE -> !alu_eq,
    (BranchCond.LT, BranchCond.LTU) -> alu_less,
    (BranchCond.GE, BranchCond.GEU) -> !alu_less
  )

  val next_pc = exe_ic.pc_next_sel.mux(
    PCNextSel.seq -> pc_plus_4,
    PCNextSel.jalr -> io.d2c.alu_ret.asUInt,
    PCNextSel.jal -> io.d2c.pcpi,
    PCNextSel.br -> Mux(should_br, io.d2c.pcpi, exe_pc + 4)
  )

  pc := next_pc


  // MEM stage
  val mem_ir = Reg(Bits(conf.xlen bits)) init(Misc.NOP)
  mem_ir := exe_ir
  val mem_pc = RegNext(exe_pc)
  val mem_ic = Reg(new InstructionCtrl()) init(InstructionCtrl(conf, Misc.NOP, U(0)))
  mem_ic := exe_ic

  io.c2d.wen := mem_ic.dwen
  io.c2d.store_type := mem_ic.store_type

  // WB stage
  val wb_ir = Reg(Bits(conf.xlen bits)) init(Misc.NOP)
  wb_ir := mem_ir
  val wb_pc = RegNext(mem_pc)
  val wb_ic = Reg(new InstructionCtrl()) init(InstructionCtrl(conf, Misc.NOP, U(0)))
  wb_ic := mem_ic

  io.c2d.rd := wb_ic.rd
  io.c2d.reg_wen := wb_ic.reg_wen
  io.c2d.wb_sel := wb_ic.wb_sel
  io.c2d.load_type := wb_ic.load_type

  // Stall
  val stall = Bool()
  stall := (dec_ic.rs1 =/= U(0)) && (dec_ic.rs2 =/= U(0))

}
