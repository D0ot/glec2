package glec2

import spinal.core._
import spinal.lib._

object Misc {
  def NOP = B(0x13, 32 bits)
}

object InsOpcode {
  def LUI       = M"-------------------------0110111"
  def AUIPC     = M"-------------------------0010111"
  def JAL       = M"-------------------------1101111"
  def JALR      = M"-------------------------1100111"
  def B_BASE    = M"-------------------------1100011"
  def L_BASE    = M"-------------------------0000011"
  def S_BASE    = M"-------------------------0100011"
  def ALI_BASE  = M"-------------------------0010011"
  def ALR_BASE  = M"-------------------------0110011"
}

class IMM(implicit conf : CoreParams) extends Bundle {
  val i_imm = Bits(12 bits)
  val s_imm = Bits(12 bits)
  val b_imm = Bits(13 bits)
  val u_imm = Bits(32 bits)
  val j_imm = Bits(21 bits)

  val i_sext = Bits(32 bits)
  val s_sext = Bits(32 bits)
  val b_sext = Bits(32 bits)
  val j_sext = Bits(32 bits)

  def load(ins : Bits) {
    i_imm := ins(31 downto 20)
    s_imm := ins(31 downto 25) ## ins(11 downto 7)
    b_imm := ins(31) ## ins(7) ## ins(30 downto 25) ## ins(11 downto 8) ## False
    u_imm := ins(31 downto 12) ## U"x000"
    j_imm := ins(31) ## ins(19 downto 12) ## ins(20) ## ins(30 downto 21) ## False

    i_sext := sign_ext_to(i_imm, 32)
    s_sext := sign_ext_to(s_imm, 32)
    b_sext := sign_ext_to(b_imm, 32)
    j_sext := sign_ext_to(j_imm, 32)
  }

  def sign_ext_to(imm : Bits, ret_width : Int):Bits = {
    val width = imm.getWidth
    val start = ret_width - width - 1
    val ret = Bits(ret_width bits)
    ret := B((start downto 0) -> imm.msb) ## imm
    ret
  }


}

object IMM {
  def apply(implicit conf : CoreParams, ins : Bits):IMM = {
    val imm = new IMM()
    imm.load(ins)
    imm
  }

  def apply(implicit conf : CoreParams):IMM = {
    new IMM()
  }
}


object BranchCond extends SpinalEnum {
  val EQ, NE, LT, GE, LTU, GEU = newElement()
  defaultEncoding = SpinalEnumEncoding("staticEncoding")(
    EQ -> 0,
    NE -> 1,
    LT -> 4,
    GE -> 5,
    LTU -> 6,
    GEU -> 7
  )
}

object PCNextSel extends SpinalEnum{
  val seq, jalr, br, jal = newElement()
}

object ALUOp1Sel extends SpinalEnum {
  val reg, zero = newElement()
}

object ALUOp2Sel extends SpinalEnum {
  val reg, imm = newElement()
}

// pcpi is "PC Plus IMM"
object WriteBackSel extends SpinalEnum {
  val alu, mem, pp4, pcpi = newElement()
}

object LoadType extends SpinalEnum {
  val word, shalf, uhalf, sbyte, ubyte = newElement()
  defaultEncoding = SpinalEnumEncoding("staticEncoding")(
    sbyte -> 0,
    shalf -> 1,
    word -> 2,
    ubyte -> 4,
    uhalf -> 5
  )
}

object StoreType extends SpinalEnum {
  val word, half, byte, dummy = newElement()
  defaultEncoding = SpinalEnumEncoding("staticEncoding")(
    byte -> 0,
    half -> 1,
    word -> 2,
    
    dummy -> 5
  )
}

class InstructionCtrl(implicit conf : CoreParams) extends Bundle{
  val ins = Bits(conf.xlen bits)
  val pc = UInt(conf.xlen bits)
  val rs1 = UInt(5 bits)
  val rs2 = UInt(5 bits)
  val rd = UInt(5 bits)
  val funct3 = Bits(3 bits)
  val ins_bit30 = Bool()
  val imm = Bits(32 bits)
  
  val pre_imm = new IMM()
  
  val pc_next_sel = PCNextSel()
  val is_branch = Bool()
  val bc = BranchCond()

  val alu_opcode = ALUOpcode()
  val alu_op1_sel = ALUOp1Sel()
  val alu_op2_sel = ALUOp2Sel()
  val reg_wen = Bool()

  val wb_sel = WriteBackSel()

  val load_type = LoadType()
  val store_type = StoreType()
  val dwen = Bool()

  val invalid = Bool()
}

object InstructionCtrl {
  def apply(implicit conf : CoreParams, ins: Bits, pc: UInt) : InstructionCtrl = {
    val ins_ctrl = new InstructionCtrl()
    ins_ctrl.pc := pc
    ins_ctrl.ins := ins

    ins_ctrl.rs1 := ins(19 downto 15).asUInt
    ins_ctrl.rs2 := ins(24 downto 20).asUInt
    ins_ctrl.rd := ins(11 downto 7).asUInt
    ins_ctrl.funct3 := ins(14 downto 12).asBits
    ins_ctrl.ins_bit30 := ins(30)
    ins_ctrl.bc := BranchCond.EQ
    ins_ctrl.pre_imm.load(ins)
    ins_ctrl.imm := ins_ctrl.pre_imm.i_sext

    ins_ctrl.pc_next_sel := PCNextSel.seq

    ins_ctrl.alu_opcode := ALUOpcode.ADD
    ins_ctrl.alu_op1_sel := ALUOp1Sel.reg
    ins_ctrl.alu_op2_sel := ALUOp2Sel.reg
    ins_ctrl.wb_sel := WriteBackSel.alu
    ins_ctrl.reg_wen := True


    ins_ctrl.load_type := LoadType.word
    ins_ctrl.store_type := StoreType.word
    ins_ctrl.dwen := False

    ins_ctrl.invalid := False
    when(ins === InsOpcode.LUI) {
      ins_ctrl.alu_op1_sel := ALUOp1Sel.zero
      ins_ctrl.alu_op2_sel := ALUOp2Sel.imm
      ins_ctrl.imm := ins_ctrl.pre_imm.u_imm
      
    } elsewhen(ins === InsOpcode.AUIPC) {
      // ALU need not to work
      ins_ctrl.imm := ins_ctrl.pre_imm.u_imm
      ins_ctrl.wb_sel := WriteBackSel.pcpi

    } elsewhen(ins === InsOpcode.JAL) {
      ins_ctrl.imm := ins_ctrl.pre_imm.j_sext
      ins_ctrl.wb_sel := WriteBackSel.pp4
      ins_ctrl.pc_next_sel := PCNextSel.jal

    } elsewhen(ins === InsOpcode.JALR) {
      ins_ctrl.alu_op2_sel := ALUOp2Sel.imm
      ins_ctrl.imm := ins_ctrl.pre_imm.i_sext
      ins_ctrl.wb_sel := WriteBackSel.pp4
      ins_ctrl.pc_next_sel := PCNextSel.jalr

    } elsewhen(ins === InsOpcode.B_BASE) {
      // result of ALU can be used to determine if we can branch
      ins_ctrl.alu_opcode := ALUOpcode.SLT
      ins_ctrl.imm := ins_ctrl.pre_imm.b_sext
      ins_ctrl.bc.assignFromBits(ins_ctrl.funct3)
      ins_ctrl.pc_next_sel := PCNextSel.br
      ins_ctrl.reg_wen := False

    } elsewhen(ins === InsOpcode.L_BASE) {
      ins_ctrl.alu_op2_sel := ALUOp2Sel.imm
      ins_ctrl.imm := ins_ctrl.pre_imm.i_sext
      ins_ctrl.wb_sel := WriteBackSel.mem
      ins_ctrl.load_type.assignFromBits(ins_ctrl.funct3)

    } elsewhen(ins === InsOpcode.S_BASE) {
      ins_ctrl.alu_op2_sel := ALUOp2Sel.imm
      ins_ctrl.imm := ins_ctrl.pre_imm.s_sext
      ins_ctrl.dwen := True
      ins_ctrl.reg_wen := False
      ins_ctrl.store_type.assignFromBits(ins_ctrl.funct3)

    } elsewhen(ins === InsOpcode.ALI_BASE) {
      ins_ctrl.alu_op2_sel := ALUOp2Sel.imm
      ins_ctrl.imm := ins_ctrl.pre_imm.i_sext
      ins_ctrl.alu_opcode.assignFromBits(ins_ctrl.funct3)

    } elsewhen(ins === InsOpcode.ALR_BASE) {
      ins_ctrl.alu_opcode.assignFromBits(ins_ctrl.funct3)

    } otherwise {
      ins_ctrl.invalid := True
    }

    ins_ctrl
  }
}
