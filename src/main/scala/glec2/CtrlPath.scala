package glec2

import spinal.core._
import spinal.lib._

// select the data from which is forwarded
object ForwordSel extends SpinalEnum {
  val none, exe, mem, wb = newElement()
}

case class ICacheCmd(implicit conf : CoreParams) extends Bundle with IMasterSlave {
  val pc = UInt(conf.pcWidth bits)
  
  def asMaster(): Unit = {
    out(pc)
  }
}

object ICacheCmd {
  def apply(implicit conf : CoreParams, pc : UInt) : ICacheCmd = {
    val cmd = ICacheCmd()
    cmd.pc := pc
    cmd
  }
}

case class ICacheRsp(implicit conf : CoreParams) extends Bundle with IMasterSlave {
  val pc = UInt(conf.pcWidth bits)
  val ins = Bits(32 bits)

  def asMaster(): Unit = {
    in(pc)
    in(ins)
  }
}

case class ICacheBus(implicit conf : CoreParams) extends Bundle with IMasterSlave {
  val cmd = Stream(ICacheCmd())
  val rsp = Flow(ICacheRsp())
  def asMaster(): Unit = {
    master (cmd)
    slave (rsp)
  }
}

case class CtrlInspect(implicit conf : CoreParams) extends Bundle with IMasterSlave {
  val if_pc = UInt(conf.pcWidth bits)
  val dec_pc = UInt(conf.pcWidth bits)
  val exe_pc = UInt(conf.pcWidth bits)
  val mem_pc = UInt(conf.pcWidth bits)
  val wb_pc = UInt(conf.pcWidth bits)

  val dec_ir = Bits(32 bits)
  val exe_ir = Bits(32 bits)
  val mem_ir = Bits(32 bits)
  val wb_ir = Bits(32 bits)

  def asMaster(): Unit = {
    out(if_pc)
    out(dec_pc)
    out(exe_pc)
    out(mem_pc)
    out(wb_pc)

    out(dec_ir)
    out(exe_ir)
    out(mem_ir)
    out(wb_ir)
  }

}

case class Ctrl2DataIO(implicit conf : CoreParams) extends Bundle with IMasterSlave{

  // ID stage
  val rs1 = UInt(5 bits)
  val rs2 = UInt(5 bits)
  val imm = Bits(32 bits)
  val alu_op1_sel = ALUOp1Sel()
  val alu_op2_sel = ALUOp2Sel()
  val dec_pc = UInt(conf.pcWidth bits)

  val fwd1_sel = ForwordSel()
  val fwd2_sel = ForwordSel()

  val fwd1_wb_sel = WriteBackSel()
  val fwd2_wb_sel = WriteBackSel()

  // EXE stage
  val alu_opcode = ALUOpcode()
  val ins_bit30 = Bool()
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
    out(dec_pc)
    out(fwd1_sel)
    out(fwd2_sel)
    out(fwd1_wb_sel)
    out(fwd2_wb_sel)

    out(alu_opcode)
    out(ins_bit30)
    out(do_sub)

    out(wen)
    out(store_type)
    out(load_type)

    out(rd)
    out(reg_wen)
    out(wb_sel)
  }
}

// fetch decode execute memory writeback
// fet, dec, exe, mem, wb
case class CtrlPath(implicit conf : CoreParams) extends Component {
  val io = new Bundle {
    val icb = master (ICacheBus())
    val c2d = master (Ctrl2DataIO())
    val d2c = slave (Data2CtrlIO())

    val inspect = master (CtrlInspect())
  }

  val should_jal = Bool()
  val should_jalr = Bool()
  val should_branch = Bool()

  // IF stage
  // it will be high after reset
  val rest_done = RegNext(True) init(False)

  val stall = Bool()

  val inc = False
  val pc = Reg(UInt(conf.pcWidth bits)) init(conf.pcInitVal)
  val load_pc = UInt()
  val should_load_pc = Bool()

  val next_pc = Mux(should_load_pc, load_pc, 
        Mux(inc, pc + U(4), pc))

  pc := next_pc
  io.icb.cmd.payload.pc := next_pc
  io.icb.cmd.valid := rest_done 

  // ID stage
  val dec_ir = Reg(Bits(32 bits)) init(Misc.NOP)
  val dec_pc = Reg(UInt(conf.xlen bits)) init(conf.pcInitVal)
  val dec_ic = InstructionCtrl(conf, dec_ir, dec_pc)


  when(stall) {
    dec_ir := dec_ir
    dec_pc := dec_pc
    inc := False
  } otherwise {
    when(io.icb.rsp.fire && !should_load_pc) {
      dec_ir := io.icb.rsp.ins
      dec_pc := io.icb.rsp.pc
      inc := True
    } otherwise {
      dec_ir := Misc.NOP
      dec_pc := U(0)
      inc := False
    }

  }

  io.c2d.rs1 := dec_ic.rs1
  io.c2d.rs2 := dec_ic.rs2
  io.c2d.imm := dec_ic.imm
  io.c2d.dec_pc := dec_pc

  io.c2d.alu_op1_sel := dec_ic.alu_op1_sel
  io.c2d.alu_op2_sel := dec_ic.alu_op2_sel

  // EXE stage
  val exe_ir = Reg(Bits(conf.xlen bits)) init(Misc.NOP)
  exe_ir := Mux(stall || should_branch, Misc.NOP, dec_ir)

  val exe_pc = Reg(UInt(conf.pcWidth bits)) init(conf.pcInitVal)
  exe_pc := Mux(stall || should_branch, U(0), dec_pc)

  val exe_ic = Reg(InstructionCtrl()) init(InstructionCtrl(conf, Misc.NOP, U(0)))
  exe_ic := Mux(stall || should_branch, InstructionCtrl(conf, Misc.NOP, U(0)), dec_ic)

  io.c2d.alu_opcode := exe_ic.alu_opcode
  io.c2d.ins_bit30 := exe_ic.ins_bit30
  io.c2d.do_sub := exe_ic.alu_do_sub


  // Jump and Link, Branch 
  should_jal := PCNextSel.jal === dec_ic.pc_next_sel
  should_jalr := PCNextSel.jalr === dec_ic.pc_next_sel

  val alu_eq = io.d2c.alu_ret === B(0, conf.xlen bits)
  val alu_less = io.d2c.alu_ret(0)

  val branch_take = exe_ic.bc.mux(
    BranchCond.EQ -> alu_eq,
    BranchCond.NE -> !alu_eq,
    (BranchCond.LT, BranchCond.LTU) -> alu_less,
    (BranchCond.GE, BranchCond.GEU) -> !alu_less
  )
  should_branch := (PCNextSel.br === exe_ic.pc_next_sel) && (branch_take)

  should_load_pc := True
  when(should_jal === True) {
    load_pc := io.d2c.dec_pcpi
  } elsewhen(should_jalr === True) {
    load_pc := io.d2c.dec_immpr
  } elsewhen(should_branch === True) {
    load_pc := io.d2c.exe_pcpi
  } otherwise {
    load_pc := U(0)
    should_load_pc := False
  }

 


  // MEM stage
  val mem_ir = Reg(Bits(conf.xlen bits)) init(Misc.NOP)
  mem_ir := exe_ir
  val mem_pc = RegNext(exe_pc) init(U(0))
  val mem_ic = Reg(InstructionCtrl()) init(InstructionCtrl(conf, Misc.NOP, U(0)))
  mem_ic := exe_ic

  io.c2d.wen := mem_ic.dwen
  io.c2d.store_type := mem_ic.store_type

  // WB stage
  val wb_ir = Reg(Bits(conf.xlen bits)) init(Misc.NOP)
  wb_ir := mem_ir
  val wb_pc = RegNext(mem_pc) init(U(0))
  val wb_ic = Reg(InstructionCtrl()) init(InstructionCtrl(conf, Misc.NOP, U(0)))
  wb_ic := mem_ic

  io.c2d.rd := wb_ic.rd
  io.c2d.reg_wen := wb_ic.reg_wen
  io.c2d.wb_sel := wb_ic.wb_sel
  io.c2d.load_type := wb_ic.load_type

  // Stall
  val dec_use_rs1 = dec_ic.alu_op1_sel === ALUOp1Sel.reg

  // because the write data to DCache is from rs2, so "|| (dec_ic.dwen)"
  // ALUOp2Sel is just about ALU operand
  val dec_use_rs2 = (dec_ic.alu_op2_sel === ALUOp2Sel.reg) || (dec_ic.dwen)
  
  if(conf.bypass) {
    // only stall when source register is writen by a load instruction
    // because the DCache has one cycle latency, 
    // if it is a load instruction, we can only bypass in WB stage
    stall := 
      ((exe_ic.rd === dec_ic.rs1) && (dec_ic.rs1 =/= U(0)) && dec_use_rs1 && exe_ic.is_load) ||
      ((exe_ic.rd === dec_ic.rs2) && (dec_ic.rs2 =/= U(0)) && dec_use_rs2 && exe_ic.is_load) ||
      ((mem_ic.rd === dec_ic.rs1) && (dec_ic.rs1 =/= U(0)) && dec_use_rs1 && mem_ic.is_load) ||
      ((mem_ic.rd === dec_ic.rs2) && (dec_ic.rs2 =/= U(0)) && dec_use_rs2 && mem_ic.is_load)

    val fwd1_sel = ForwordSel()
    fwd1_sel := ForwordSel.none
    io.c2d.fwd1_wb_sel := WriteBackSel.alu

    when(!dec_use_rs1 || dec_ic.rs1 === U(0)) {
      fwd1_sel := ForwordSel.none

    } elsewhen(dec_ic.rs1 === exe_ic.rd && exe_ic.reg_wen) {
      fwd1_sel := ForwordSel.exe
      io.c2d.fwd1_wb_sel := exe_ic.wb_sel

    } elsewhen(dec_ic.rs1 === mem_ic.rd && mem_ic.reg_wen) {
      fwd1_sel := ForwordSel.mem
      io.c2d.fwd1_wb_sel := mem_ic.wb_sel

    } elsewhen(dec_ic.rs1 === wb_ic.rd && wb_ic.reg_wen) {
      fwd1_sel := ForwordSel.wb
    } otherwise {
      fwd1_sel := ForwordSel.none
    }

    val fwd2_sel = ForwordSel()
    fwd2_sel := ForwordSel.none
    io.c2d.fwd2_wb_sel := WriteBackSel.alu

    when(!dec_use_rs2 || dec_ic.rs2 === U(0)) {
      fwd2_sel := ForwordSel.none

    } elsewhen(dec_ic.rs2 === exe_ic.rd && exe_ic.reg_wen) {
      fwd2_sel := ForwordSel.exe
      io.c2d.fwd2_wb_sel := exe_ic.wb_sel

    } elsewhen(dec_ic.rs2 === mem_ic.rd && mem_ic.reg_wen) {
      fwd2_sel := ForwordSel.mem
      io.c2d.fwd2_wb_sel := mem_ic.wb_sel

    } elsewhen(dec_ic.rs2 === wb_ic.rd && wb_ic.reg_wen) {
      fwd2_sel := ForwordSel.wb
    } otherwise {
      fwd2_sel := ForwordSel.none
    }
    
    io.c2d.fwd1_sel := fwd1_sel
    io.c2d.fwd2_sel := fwd2_sel
    
  } else {
    // use full stall logic
    stall :=
      ((exe_ic.rd === dec_ic.rs1) && (dec_ic.rs1 =/= U(0)) && dec_use_rs1) ||
      ((exe_ic.rd === dec_ic.rs2) && (dec_ic.rs2 =/= U(0)) && dec_use_rs2) || 
      ((mem_ic.rd === dec_ic.rs1) && (dec_ic.rs1 =/= U(0)) && dec_use_rs1) ||
      ((mem_ic.rd === dec_ic.rs2) && (dec_ic.rs2 =/= U(0)) && dec_use_rs2) ||
      ((wb_ic.rd === dec_ic.rs1) && (dec_ic.rs1 =/= U(0)) && dec_use_rs1) ||
      ((wb_ic.rd === dec_ic.rs2) && (dec_ic.rs2 =/= U(0)) && dec_use_rs2)
  }

  io.inspect.if_pc := pc
  io.inspect.dec_pc := dec_pc
  io.inspect.exe_pc := exe_pc
  io.inspect.mem_pc := mem_pc
  io.inspect.wb_pc := wb_pc

  io.inspect.dec_ir := dec_ir
  io.inspect.exe_ir := exe_ir
  io.inspect.mem_ir := mem_ir 
  io.inspect.wb_ir := wb_ir


}
