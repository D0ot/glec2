package glec2

import spinal.core._
import spinal.lib._

case class DCacheBus (implicit conf : CoreParams) extends Bundle with IMasterSlave{
  val addr = UInt(conf.xlen bits)
  val wen = Bool()
  val rdata = Bits(conf.xlen bits)
  val wdata = Bits(conf.xlen bits)

  def asMaster(): Unit = {
    out (addr)
    out (wen)
    out (wdata)
    in (rdata)
  }
}

case class Data2CtrlIO (implicit conf : CoreParams) extends Bundle with IMasterSlave{
  def asMaster(): Unit = {
  }
}

case class DataPath (implicit conf : CoreParams) extends Component {
  val io = new Bundle {
    val dbus = master (DCacheBus())
    val d2c = master (Data2CtrlIO())
    val c2d = slave (Ctrl2DataIO())
  }
}
