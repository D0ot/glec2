package glec2

import spinal.core._
import spinal.lib._

object ALUOpcode extends SpinalEnum(binarySequential) {
  val ADD, SLL, SLT, SLTU, XOR, SRL, OR, AND = newElement()
}

case class ALUIO(implicit conf : CoreParams) extends Bundle {
  val opcode = in (ALUOpcode)
  val op1 = in Bits(conf.xlen bits)
  val op2 = in Bits(conf.xlen bits)
  val result = out Bits(conf.xlen bits)
  val add = out Bits(conf.xlen bits)
  
  // bit 30 in an instruction
  // for ADD opcode, bit30 == 0 -> ADD
  //                 bit30 == 1 -> SUB
  // for SRL opcode, bit30 == 0 -> SRL
  //                 bit30 == 1 -> SRA
  val bit30 = in Bool()
}


case class ALU(implicit conf : CoreParams) extends Component {
  val io = ALUIO()

  val doSub = (io.opcode === ALUOpcode.SLT) || 
    (io.opcode === ALUOpcode.SLTU) ||
    ((io.opcode === ALUOpcode.ADD) && io.bit30)

  val add = io.op1.asSInt + Mux(doSub, ~io.op2.asSInt, io.op2.asSInt) + Mux(doSub, S(1), S(0))
  
  val less = Mux(io.op1.msb === io.op2.msb, add.msb,
    Mux(io.opcode === ALUOpcode.SLTU, io.op2.msb, io.op1.msb))

  val shamt = io.op2(4 downto 0).asUInt


  io.result := io.opcode.mux(
    ALUOpcode.ADD -> (add.asBits),
    ALUOpcode.AND -> (io.op1 & io.op2),
    ALUOpcode.OR -> (io.op1 | io.op2),
    ALUOpcode.XOR -> (io.op1 ^ io.op2),
    ALUOpcode.SLL -> (io.op1 |<< shamt),
    (ALUOpcode.SLT, ALUOpcode.SLTU) -> less.asBits.resized,
    ALUOpcode.SRL -> (Mux(io.bit30, (io.op1.asSInt >> shamt), (io.op1.asSInt |>> shamt))).asBits
  )

  io.add := add.asBits
}
