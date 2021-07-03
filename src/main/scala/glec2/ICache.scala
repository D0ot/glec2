package glec2

import spinal.core._
import spinal.lib._
import glec2.lib.HexReader


case class CoreInstructionCmd (implicit conf : CoreParams) extends Bundle {
  val pc = UInt(conf.pcWidth bits)
}

case class CoreInstructionRsp (implicit conf : CoreParams) extends Bundle {
  val pc = UInt(conf.pcWidth bits)
  val ins = Bits(conf.xlen bits)
}

case class InstructionBus (implicit conf : CoreParams) extends Bundle with IMasterSlave{
  val cmd = Stream(CoreInstructionCmd())
  val rsp = Stream(CoreInstructionRsp())
  
  def asMaster(): Unit = {
    cmd.asMaster()
    rsp.asSlave()
  }
}

case class ICache(implicit conf : CoreParams) extends Component {
  val io = new Bundle {
    val icb = slave (ICacheBus())
  }
  val insPath = "./riscv/program.bin"
  //val icache = Mem(Bits(conf.xlen bits), conf.l1cacheSize)
  val icache = Mem(UInt(conf.xlen bits), HexReader.loadInsToUInt(insPath))

  val pc = Reg(UInt(conf.pcWidth bits)) init(U(0))
  
  // always ready, because it is a fake cache...
  io.icb.cmd.ready := True

  val transfered = RegNext(io.icb.cmd.fire) init(False)
  val rdat = Bits(conf.xlen bits)
  val mem_access  = icache.readSync((io.icb.cmd.payload.pc |>> 2).resized, io.icb.cmd.valid).asBits

  when(io.icb.cmd.fire) {
    pc := io.icb.cmd.payload.pc
    rdat := mem_access
  } otherwise {
    rdat := B(0)
  }

  io.icb.rsp.valid := transfered
  io.icb.rsp.payload.pc := pc
  io.icb.rsp.payload.ins := rdat
  
}
