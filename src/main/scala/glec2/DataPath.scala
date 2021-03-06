package glec2

import spinal.core._
import spinal.lib._

case class DCacheBus (implicit conf : CoreParams) extends Bundle with IMasterSlave{
  val addr = UInt(conf.xlen bits)
  val wen = Bool()
  val rdata = Bits(conf.xlen bits)
  val wdata = Bits(conf.xlen bits)
  val store_type = StoreType()
  val load_type = LoadType()

  def asMaster(): Unit = {
    out (addr)
    out (wen)
    out (wdata)
    out (store_type)
    in (rdata)
    out (load_type)
  }
}

case class Data2CtrlIO (implicit conf : CoreParams) extends Bundle with IMasterSlave{

  // ID stage
  // pc plus imm
  val dec_pcpi = UInt(conf.pcWidth bits)
  // pc plus reg
  val dec_immpr = UInt(conf.pcWidth bits)
  
  // EXE stage
  val alu_ret = Bits(conf.xlen bits)
  val exe_pcpi = UInt(conf.pcWidth bits)


  def asMaster(): Unit = {
    out(dec_pcpi)
    out(dec_immpr)
    out(alu_ret)
    out(exe_pcpi)
  }
}

case class DataPath (implicit conf : CoreParams) extends Component {
  val io = new Bundle {
    val dcb = master (DCacheBus())
    val d2c = master (Data2CtrlIO())
    val c2d = slave (Ctrl2DataIO())

    val data2pm = master(PerfMonitorDataIO())
  }



  val regFile = RegFile()
  regFile.io.rs1_addr := io.c2d.rs1
  regFile.io.rs2_addr := io.c2d.rs2
  regFile.io.waddr := io.c2d.rd
  regFile.io.wen := io.c2d.reg_wen

  regFile.io.dm_wen := False
  regFile.io.dm_addr := U(0)
  regFile.io.dm_wdata := B(0)

  val reg1_bypassed = Bits(conf.xlen bits)
  val reg2_bypassed = Bits(conf.xlen bits)

  val pcpi = io.c2d.dec_pc + io.c2d.imm.asUInt
  io.d2c.dec_pcpi := pcpi

  val immpr = io.c2d.imm.asUInt + reg1_bypassed.asUInt
  io.d2c.dec_immpr := immpr

  val exe_alu_op1 = Reg(Bits(conf.xlen bits))
  val exe_alu_op2 = Reg(Bits(conf.xlen bits))
  val exe_wdat = Reg(Bits(conf.xlen bits))
  val exe_pcpi = Reg(UInt(conf.xlen bits))
  val exe_pc = Reg(UInt(conf.pcWidth bits))
  io.d2c.exe_pcpi := exe_pcpi


  exe_wdat := reg2_bypassed
  exe_pcpi := pcpi
  exe_pc := io.c2d.dec_pc

  exe_alu_op1 := io.c2d.alu_op1_sel.mux(
      ALUOp1Sel.reg -> reg1_bypassed,
      ALUOp1Sel.zero -> B(0, conf.xlen bits)
  )

  exe_alu_op2 := io.c2d.alu_op2_sel.mux(
      ALUOp2Sel.reg -> reg2_bypassed,
      ALUOp2Sel.imm -> io.c2d.imm
  )

  val alu = ALU()

  alu.io.op1 := exe_alu_op1
  alu.io.op2 := exe_alu_op2
  alu.io.opcode := io.c2d.alu_opcode
  alu.io.bit30 := io.c2d.ins_bit30
  alu.io.doSub := io.c2d.do_sub

  io.d2c.alu_ret := alu.io.result


  val mem_alu_ret = Reg(Bits(conf.xlen bits))
  val mem_pcpi = Reg(UInt(conf.xlen bits))
  val mem_wdat = Reg(Bits(conf.xlen bits))
  val mem_pc = Reg(UInt(conf.pcWidth bits))
  io.dcb.store_type := io.c2d.store_type
  io.dcb.load_type := io.c2d.load_type

  
  mem_alu_ret := alu.io.result
  mem_pcpi := exe_pcpi
  mem_wdat := exe_wdat
  mem_pc := exe_pc

  io.dcb.addr := mem_alu_ret.asUInt
  io.dcb.wdata := mem_wdat
  io.dcb.wen := io.c2d.wen


  val wb_alu_ret = Reg(Bits(conf.xlen bits))
  wb_alu_ret := mem_alu_ret
  val wb_pcpi = Reg(UInt(conf.xlen bits))
  wb_pcpi := mem_pcpi
  val wb_pc = Reg(UInt(conf.xlen bits))
  wb_pc := mem_pc
  val wb_dat = Bits(conf.xlen bits)
  
  val wb_mem_addr = wb_alu_ret

  def sign_width_process(dat : Bits, load_type : SpinalEnumCraft[LoadType.type]):Bits = {

    def uext(dat : Bits) = B(0, (32 - dat.getWidth) bits) ## dat
    def sext(dat : Bits) = B((32 - dat.getWidth - 1 downto 0) -> dat.msb) ## dat
    load_type.mux(
        LoadType.word -> (dat),
        LoadType.ubyte -> wb_mem_addr(1 downto 0).mux(
          B(3) -> uext(dat(31 downto 24)),
          B(2) -> uext(dat(23 downto 16)),
          B(1) -> uext(dat(15 downto 8)),
          B(0) -> uext(dat(7 downto 0))
          ),
        LoadType.sbyte -> wb_mem_addr(1 downto 0).mux(
          B(3) -> sext(dat(31 downto 24)),
          B(2) -> sext(dat(23 downto 16)),
          B(1) -> sext(dat(15 downto 8)),
          B(0) -> sext(dat(7 downto 0))
          ),
        LoadType.uhalf -> wb_mem_addr(1).mux(
          False -> uext(dat(15 downto 0)),
          True -> uext(dat(31 downto 16))
          ),
        LoadType.shalf -> wb_mem_addr(1).mux(
          False -> sext(dat(15 downto 0)),
          True -> sext(dat(31 downto 16))
          )
      )
  }

  wb_dat := io.c2d.wb_sel.mux(
    WriteBackSel.alu -> wb_alu_ret,
    WriteBackSel.mem -> sign_width_process(io.dcb.rdata, io.c2d.load_type),
    WriteBackSel.pcpi -> wb_pcpi.asBits,
    WriteBackSel.pp4 -> (wb_pc + U(4, conf.pcWidth bits)).asBits
  )

  regFile.io.wdata := wb_dat

  if(conf.bypass) {
    reg1_bypassed := io.c2d.fwd1_sel.mux(
      ForwordSel.none -> (regFile.io.rs1_data),
      ForwordSel.exe -> ( io.c2d.fwd1_wb_sel.mux(
        WriteBackSel.alu -> (alu.io.result),
        WriteBackSel.mem -> (B(0)), // can not occur
        WriteBackSel.pcpi -> (exe_pcpi.asBits),
        WriteBackSel.pp4 -> ((exe_pc + U(4)).asBits)
      )),
      ForwordSel.mem -> ( io.c2d.fwd1_wb_sel.mux(
        WriteBackSel.alu -> (mem_alu_ret),
        WriteBackSel.mem -> (B(0)), // can not occur
        WriteBackSel.pcpi -> (mem_pcpi.asBits),
        WriteBackSel.pp4 -> ((mem_pc + U(4)).asBits)
      )),
      ForwordSel.wb -> (wb_dat)
    )

    reg2_bypassed := io.c2d.fwd2_sel.mux(
      ForwordSel.none -> (regFile.io.rs2_data),
      ForwordSel.exe -> ( io.c2d.fwd2_wb_sel.mux(
        WriteBackSel.alu -> (alu.io.result),
        WriteBackSel.mem -> (B(0)), // can not occur
        WriteBackSel.pcpi -> (exe_pcpi.asBits),
        WriteBackSel.pp4 -> ((exe_pc + U(4)).asBits)
      )),
      ForwordSel.mem -> ( io.c2d.fwd2_wb_sel.mux(
        WriteBackSel.alu -> (mem_alu_ret),
        WriteBackSel.mem -> (B(0)), // can not occur
        WriteBackSel.pcpi -> (mem_pcpi.asBits),
        WriteBackSel.pp4 -> ((mem_pc + U(4)).asBits)
      )),
      ForwordSel.wb -> (wb_dat)
    )
  } else {
    reg1_bypassed := regFile.io.rs1_data
    reg2_bypassed := regFile.io.rs2_data
  }


  val perf_monitor = new Area {
    // TODO
    io.data2pm.dcache_wait := False
  }

}
