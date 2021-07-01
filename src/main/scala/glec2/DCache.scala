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

  // address 0
  val byte0 = Mem(Bits(8 bits), conf.d1cacheSize) randBoot()

  // address 1
  val byte1 = Mem(Bits(8 bits), conf.d1cacheSize) randBoot()

  // address 2
  val byte2 = Mem(Bits(8 bits), conf.d1cacheSize) randBoot()

  // address 3
  val byte3 = Mem(Bits(8 bits), conf.d1cacheSize) randBoot()

  val real_addr = (io.dcb.addr |>> 2).resized

  val which_byte = io.dcb.addr(1 downto 0)
  val which_half = io.dcb.addr(1)

  val byte_to_write = io.dcb.wdata(7 downto 0)

  io.dcb.rdata := byte3.readAsync(real_addr) ## 
    byte2.readAsync(real_addr) ## 
    byte1.readAsync(real_addr) ## 
    byte0.readAsync(real_addr)


  when(io.dcb.wen) {
    when(io.dcb.store_type === StoreType.byte) {
      when(which_byte.asBits === B(0)) {
        byte0.write(real_addr, byte_to_write)
      } elsewhen(which_byte.asBits === B(1)) {
        byte1.write(real_addr, byte_to_write)
      } elsewhen(which_byte.asBits === B(2)) {
        byte2.write(real_addr, byte_to_write)
      } otherwise {
        byte3.write(real_addr, byte_to_write)
      }
    }

    when(io.dcb.store_type === StoreType.half) {
      when(which_half === False) {
        byte1.write(real_addr, io.dcb.wdata(15 downto 8))
        byte0.write(real_addr, io.dcb.wdata(7 downto 0))
      } otherwise {
        byte3.write(real_addr, io.dcb.wdata(15 downto 8))
        byte2.write(real_addr, io.dcb.wdata(7 downto 0))
      }
    }

    when(io.dcb.store_type === StoreType.word) {
      byte3.write(real_addr, io.dcb.wdata(31 downto 24))
      byte2.write(real_addr, io.dcb.wdata(23 downto 16))
      byte1.write(real_addr, io.dcb.wdata(15 downto 8))
      byte0.write(real_addr, io.dcb.wdata(7  downto 0))
    }
  } 
}
