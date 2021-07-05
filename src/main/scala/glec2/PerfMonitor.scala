package glec2

import spinal.core._
import spinal.lib._

case class PerfMonitorCtrlIO(implicit conf : CoreParams) extends Bundle with IMasterSlave{
  val icache_wait = Bool()
  val valid_wb = Bool()
  val invalid_wb = Bool()
  val bubble_wb = Bool()

  def asMaster(): Unit = {
    out(icache_wait)
    out(valid_wb)
    out(invalid_wb)
    out(bubble_wb)
  }
}

case class PerfMonitorDataIO(implicit conf : CoreParams) extends Bundle with IMasterSlave{
  val dcache_wait = Bool()

  def asMaster(): Unit = {
    out(dcache_wait)
  }

}

case class PerfMonitorIO(implicit conf : CoreParams) extends Bundle with IMasterSlave{
  val ctrl2pm = PerfMonitorCtrlIO()
  val data2pm = PerfMonitorDataIO()

  def asMaster(): Unit = {
    master (ctrl2pm)
    master (data2pm)
  }
}

case class PerMonitor(implicit conf : CoreParams) extends Component {
  val io= PerfMonitorIO()

  val reg64bit = RegInit(U(0, conf.pmWidth bits))
  
  val pm_cycles = RegInit(U(0, conf.pmWidth bits)) 
  pm_cycles := pm_cycles + 1

  val pm_iwait_cycles = RegInit(U(0, conf.pmWidth bits))
  when(io.ctrl2pm.icache_wait) {
    pm_iwait_cycles := pm_iwait_cycles + 1
  }

  val pm_dwait_cycles = RegInit(U(0, conf.pmWidth bits))
  when(io.data2pm.dcache_wait) {
    pm_dwait_cycles := pm_dwait_cycles + 1
  }

  val pm_valid_wb = RegInit(U(0, conf.pmWidth bits))
  when(io.ctrl2pm.valid_wb) {
    pm_valid_wb := pm_valid_wb + 1
  }

  val pm_invalid_wb = reg64bit.clone()
  when(io.ctrl2pm.invalid_wb) {
    pm_invalid_wb := pm_invalid_wb + 1
  }

  val pm_bubble_wb = reg64bit.clone()
  when(io.ctrl2pm.bubble_wb) {
    pm_bubble_wb := pm_bubble_wb + 1
  }

}
