package glec2

import spinal.core._
import spinal.lib._


class GlecCore(implicit conf : CoreParams) extends Component{
  val io = new Bundle {
  }

  val dataPath = DataPath()
  val ctrlPath = CtrlPath()

  val icache = ICache()
  val dcache = DCache()

  dataPath.io.c2d <> ctrlPath.io.c2d
  dataPath.io.d2c <> ctrlPath.io.d2c

  ctrlPath.io.icb <> icache.io.icb
  dataPath.io.dcb <> dcache.io.dcb
  
}
