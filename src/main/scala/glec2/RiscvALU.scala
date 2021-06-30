package glec2

import spinal.core._
import spinal.lib._

object ALUOpcode extends SpinalEnum(binarySequential) {
  val ADD, SLL, SLT, SLTU, XOR, SRL, OR, AND = newElement()
}

case class ALUIO(implicit conf : CoreParams) extends Bundle {
  val opcode = in (ALUOpcode)
  val rs1 = in Bits(conf.xlen bits)
  val rs2 = in Bits(conf.xlen bits)
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

  val add = io.rs1.asSInt + Mux(io.bit30, ~io.rs2.asSInt, io.rs2.asSInt) + Mux(io.bit30, S(1), S(0))
  
  val less = Mux(io.rs1.msb === io.rs2.msb, add.msb,
    Mux(io.opcode === ALUOpcode.SLTU, io.rs2.msb, io.rs1.msb))

  val shamt = io.rs2(4 downto 0).asUInt


  io.result := io.opcode.mux(
    ALUOpcode.ADD -> (add.asBits),
    ALUOpcode.AND -> (io.rs1 & io.rs2),
    ALUOpcode.OR -> (io.rs1 | io.rs2),
    ALUOpcode.XOR -> (io.rs1 ^ io.rs2),
    ALUOpcode.SLL -> (io.rs1 |<< shamt),
    (ALUOpcode.SLT, ALUOpcode.SLTU) -> less.asBits.resized,
    ALUOpcode.SRL -> (Mux(io.bit30, (io.rs1.asSInt >> shamt), (io.rs1.asSInt |>> shamt))).asBits
  )

  io.add := add.asBits
}
