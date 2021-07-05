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
  val icache = Mem(UInt(conf.xlen bits), HexReader.loadInsToUInt(insPath, conf.l1cacheSize))

  val cmd_pc = Reg(UInt(conf.pcWidth bits)) init(U(0))
  
  // always ready, because it is a fake cache...
  io.icb.cmd.ready := True

  // equals "io.icb.cmd.fire" of previous cycle
  val cmd_last_fire = RegInit(False)
  val rdat = Bits(conf.xlen bits)
  val last_rdat = Reg(Bits(conf.xlen bits)) init(B(0))

  // to emulate cache behavior
  val pending = RegInit(False)
  val counter = Reg(UInt(2 bits)) init(U(0))
  val pending_counter = Reg(UInt(2 bits))

  when(pending) {
    when(pending_counter === 3) {
      pending := False
    }
  } otherwise{
    counter := counter + 1
    when(counter === 3) {
      pending_counter := 0
      pending := True
    }
  }

  when(io.icb.cmd.fire) {
    cmd_pc := io.icb.cmd.pc
    cmd_last_fire := True

    rdat := icache.readSync((io.icb.cmd.payload.pc |>> 2).resized, io.icb.cmd.valid).asBits
  } otherwise {
    cmd_last_fire := False
    rdat := last_rdat
  }

  when(cmd_last_fire) {
    last_rdat := rdat
  }

  io.icb.rsp.valid := cmd_last_fire
  io.icb.rsp.payload.pc := cmd_pc
  io.icb.rsp.payload.ins := rdat
  
}
