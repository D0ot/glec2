package glec2

import spinal.core._
import spinal.lib._

case class CoreDataCmd (implicit conf : CoreParams) extends Bundle {
  val addr = UInt(conf.xlen bits)
  val wdata = UInt(conf.xlen bits)
  // write enable
  val wen = Bool()
}

case class CoreDataRsp (implicit conf : CoreParams) extends Bundle {
  val data = UInt(conf.xlen bits)
}

case class DataBus (implicit conf : CoreParams) extends Bundle with IMasterSlave{
  val cmd = Stream(CoreDataCmd())
  val rsp = Flow(CoreDataRsp())

  def asMaster(): Unit = {
    cmd.asMaster()
    rsp.asSlave()
  }
}


case class DCache(implicit conf : CoreParams) extends Component {
  val io = new Bundle {
    val dbus = slave (DCacheBus())
  }
}
