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

case class ICacheBus (implicit conf : CoreParams) extends Bundle with IMasterSlave{
  val cmd = Stream(CoreInstructionCmd())
  val rsp = Flow(CoreInstructionRsp())
  
  def asMaster(): Unit = {
    cmd.asMaster()
    rsp.asSlave()
  }
}

case class Ctrl2DataIO(implicit conf : CoreParams) extends Bundle with IMasterSlave{
  def asMaster(): Unit = {
  }
}


case class CtrlPath(implicit conf : CoreParams) extends Component {
  val io = new Bundle {
    val ibus = master (ICacheBus())
    val c2d = master (Ctrl2DataIO())
    val d2c = slave (Data2CtrlIO())
  }
}
