package glec2

import spinal.core._
import spinal.lib._

case class DCacheBus (implicit conf : CoreParams) extends Bundle with IMasterSlave{
  val addr = UInt(conf.xlen bits)
  val wen = Bool()
  val rdata = Bits(conf.xlen bits)
  val wdata = Bits(conf.xlen bits)
  val store_type = StoreType()

  def asMaster(): Unit = {
    out (addr)
    out (wen)
    out (wdata)
    out (store_type)
    in (rdata)
  }
}

case class Data2CtrlIO (implicit conf : CoreParams) extends Bundle with IMasterSlave{
  
  // exe stage
  val pcpi = UInt(conf.pcWidth bits)
  val alu_ret = Bits(conf.xlen bits)


  def asMaster(): Unit = {
    out(pcpi)
    out(alu_ret)
  }
}

case class DataPath (implicit conf : CoreParams) extends Component {
  val io = new Bundle {
    val dcb = master (DCacheBus())
    val d2c = master (Data2CtrlIO())
    val c2d = slave (Ctrl2DataIO())
  }

  val regFile = RegFile()
  regFile.io.rs1_addr := io.c2d.rs1
  regFile.io.rs2_addr := io.c2d.rs2
  regFile.io.waddr := io.c2d.rd
  regFile.io.wen := io.c2d.reg_wen

  val exe_alu_op1 = Reg(Bits(conf.xlen bits))
  val exe_alu_op2 = Reg(Bits(conf.xlen bits))
  val exe_imm = Reg(Bits(conf.xlen bits))
  val exe_wdat = Reg(Bits(conf.xlen bits))

  exe_alu_op1 := io.c2d.alu_op1_sel.mux(
    ALUOp1Sel.reg -> regFile.io.rs1_data,
    ALUOp1Sel.zero -> B(0, conf.xlen bits)
  )

  exe_alu_op2 := io.c2d.alu_op2_sel.mux(
    ALUOp2Sel.reg -> regFile.io.rs2_data,
    ALUOp2Sel.imm -> io.c2d.imm
  )

  exe_imm := io.c2d.imm
  exe_wdat := regFile.io.rs2_data

  val alu = ALU()
  alu.io.op1 := exe_alu_op1
  alu.io.op2 := exe_alu_op2
  alu.io.opcode := io.c2d.alu_opcode
  alu.io.bit30 := io.c2d.ins_bit30

  io.d2c.alu_ret := alu.io.result

  val mem_alu_ret = Reg(Bits(conf.xlen bits))
  val mem_pcpi = Reg(UInt(conf.xlen bits))
  val mem_wdat = Reg(Bits(conf.xlen bits))
  val mem_pc = Reg(UInt(conf.pcWidth bits))

  val pcpi = io.c2d.exe_pc + exe_imm.asUInt
  io.d2c.pcpi := pcpi
  
  mem_alu_ret := alu.io.result
  mem_pcpi := pcpi
  mem_wdat := exe_wdat
  mem_pc := io.c2d.exe_pc

  io.dcb.addr := mem_alu_ret.asUInt
  io.dcb.wdata := mem_wdat
  io.dcb.wen := io.c2d.wen

  val wb_dat = Reg(Bits(conf.xlen bits))

  wb_dat := io.c2d.wb_sel.mux(
    WriteBackSel.alu -> mem_alu_ret,
    WriteBackSel.mem -> io.dcb.rdata,
    WriteBackSel.pcpi -> mem_pcpi.asBits,
    WriteBackSel.pp4 -> (mem_pc + U(4, conf.pcWidth bits)).asBits
  )

  regFile.io.wdata := wb_dat
  
}
