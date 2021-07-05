package glec2

import spinal.core._
import spinal.lib._

import spinal.lib.bus.amba4.axilite.AxiLite4
import spinal.lib.soc.pinsec.Pinsec
import spinal.lib.cpu.riscv.impl.InstructionBusKind


case class GlecCoreInspect(implicit conf : CoreParams) extends Bundle with IMasterSlave{
  val pc = UInt(conf.pcWidth bits)
  val ir = Bits(32 bits)

  def asMaster(): Unit = {
    out(pc)
    out(ir)
  }
}


class GlecCore(implicit conf : CoreParams) extends Component{
  val io = new Bundle {
    val inspect = master (GlecCoreInspect())
  }

  val dataPath = DataPath()
  val ctrlPath = CtrlPath()

  val icache = ICache()
  val dcache = DCache()

  ctrlPath.io.c2d <> dataPath.io.c2d
  dataPath.io.d2c <> ctrlPath.io.d2c

  ctrlPath.io.icb <> icache.io.icb
  dataPath.io.dcb <> dcache.io.dcb
  
  io.inspect.ir := ctrlPath.io.inspect.dec_ir
  io.inspect.pc := ctrlPath.io.inspect.dec_pc
}
