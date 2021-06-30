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
    val dcb = slave (DCacheBus())
  }

  val addrStage = RegNext(io.dcb.addr, U(conf.pcInitVal))
  val wenStage = RegNext(io.dcb.wen, False)
  val wdataStage = RegNext(io.dcb.wdata)
  val dcache = Mem(Bits(conf.xlen bits), conf.d1cacheSize) randBoot()

  io.dcb.rdata := dcache.readSync(addrStage)

  when(wenStage) {
    dcache.write(addrStage, wdataStage)
  }
}
