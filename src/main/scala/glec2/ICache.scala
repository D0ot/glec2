package glec2

import spinal.core._
import spinal.lib._

case class CoreInstructionCmd (implicit conf : CoreParams) extends Bundle {
  val pc = UInt(conf.pcWidth bits)
}

case class CoreInstructionRsp (implicit conf : CoreParams) extends Bundle {
  val pc = UInt(conf.pcWidth bits)
  val ins = Bits(conf.xlen bits)
}

case class InstructionBus (implicit conf : CoreParams) extends Bundle with IMasterSlave{
  val cmd = Stream(CoreInstructionCmd())
  val rsp = Flow(CoreInstructionRsp())
  
  def asMaster(): Unit = {
    cmd.asMaster()
    rsp.asSlave()
  }
}
case class ICache(implicit conf : CoreParams) extends Component {
  val io = new Bundle {
    // instruction cache bus
    val icb = slave (ICacheBus())

  }
  val icache = Mem(Bits(conf.xlen bits), conf.l1cacheSize) randBoot()
  io.icb.ins := icache.readSync((io.icb.pc(31 downto 2)).resized)
}
