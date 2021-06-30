package glec2

import spinal.core._
import spinal.lib._

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

case class InstructionCtrl() extends Bundle{
}

