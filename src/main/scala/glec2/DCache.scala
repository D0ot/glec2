package glec2

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite.AxiLite4

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
  val b0_en = False
  val b0_dat = B(0)

  // address 1
  val byte1 = Mem(Bits(8 bits), conf.d1cacheSize) randBoot()
  val b1_en = False
  val b1_dat = B(0)

  // address 2
  val byte2 = Mem(Bits(8 bits), conf.d1cacheSize) randBoot()
  val b2_en = False
  val b2_dat = B(0)

  // address 3
  val byte3 = Mem(Bits(8 bits), conf.d1cacheSize) randBoot()
  val b3_en = False
  val b3_dat = B(0)

  val real_addr = (io.dcb.addr |>> 2).resized

  val which_byte = io.dcb.addr(1 downto 0)
  val which_half = io.dcb.addr(1)

  val byte_to_write = io.dcb.wdata(7 downto 0)

  io.dcb.rdata := byte3.readSync(real_addr) ## 
    byte2.readSync(real_addr) ## 
    byte1.readSync(real_addr) ## 
    byte0.readSync(real_addr)
  
  
  byte0.write(real_addr, b0_dat, b0_en)
  byte1.write(real_addr, b1_dat, b1_en)
  byte2.write(real_addr, b2_dat, b2_en)
  byte3.write(real_addr, b3_dat, b3_en)


  when(io.dcb.wen) {
    when(io.dcb.store_type === StoreType.byte) {
      when(which_byte.asBits === B(0)) {
        b0_dat := byte_to_write
        b0_en := True
      } elsewhen(which_byte.asBits === B(1)) {
        b1_dat := byte_to_write
        b1_en := True
      } elsewhen(which_byte.asBits === B(2)) {
        b2_dat := byte_to_write
        b2_en := True
      } otherwise {
        b3_dat := byte_to_write
        b3_en := True
      }
    }

    when(io.dcb.store_type === StoreType.half) {
      when(which_half === False) {
        b1_en := True
        b1_dat := io.dcb.wdata(15 downto 8)
        b0_en := True
        b0_dat := io.dcb.wdata(7 downto 0)
      } otherwise {
        b3_en := True
        b3_dat := io.dcb.wdata(15 downto 8)
        b2_en := True
        b2_dat := io.dcb.wdata(7 downto 0)
      }
    }

    when(io.dcb.store_type === StoreType.word) {
      b3_en := True
      b3_dat := io.dcb.wdata(31 downto 24)
      b2_en := True
      b2_dat := io.dcb.wdata(23 downto 16)

      b1_en := True
      b1_dat := io.dcb.wdata(15 downto 8)
      b0_en := True
      b0_dat := io.dcb.wdata(7 downto 0)

    }
  } 
}
