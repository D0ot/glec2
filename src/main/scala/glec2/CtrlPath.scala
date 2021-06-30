package glec2

import spinal.core._
import spinal.lib._



case class ICacheBus(implicit conf : CoreParams) extends Bundle with IMasterSlave {
  val pc = UInt(conf.pcWidth bits)
  def asMaster(): Unit = {
    out (pc)
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

  val pc = Reg(UInt(conf.pcWidth bits)) init(conf.pcInitVal)
}
