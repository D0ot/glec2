package glec2.lib

import java.io.File
import java.io.FileInputStream
import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.util.ArrayList

import spinal.core._

object HexReader {

  def readHexFile2ByteArray(path: String):Array[Byte] = {
    val file = new File(path)
    val in = new FileInputStream(file)
    val bytes = new Array[Byte](file.length().toInt)
    in.read(bytes)
    bytes
  }

  def convert2IntArray(byteArray:Array[Byte]):Array[Int] = {
    val bb = ByteBuffer.wrap(byteArray).order(ByteOrder.LITTLE_ENDIAN)
    val intCount = byteArray.length / 4
    val ret = new Array[Int](intCount)

    for(i <- 0 until intCount) {
      ret(i) = (bb.getInt())
    }
    ret
  }

  def readHexFile2IntArray(path : String):Array[Int] = {
    val byteArray = readHexFile2ByteArray(path)
    val intArray = convert2IntArray(byteArray)
    intArray
  }

  def loadInsToUInt(hexFilePath:String, len : Int):Seq[UInt] = {
    def int2LongBitwise(int:Int):Long = {
      val bitMask:Long = 0xffffffffL
      int.toLong & bitMask
    }

    var intArray = HexReader.readHexFile2IntArray(hexFilePath)
    
    if(intArray.length < len) {
      intArray = intArray ++ (for(i <- 0 until (len - intArray.length)) yield {13})
    }

    for(i <- 0 until intArray.length) yield {
      val tmpLong = int2LongBitwise(intArray(i))
      val tmpBigInt = BigInt(tmpLong)
      U(tmpBigInt, 32 bits)
    }
  }


  def loadInsToBits(hexFilePath:String):Seq[Bits] = {
    def int2LongBitwise(int:Int):Long = {
      val bitMask:Long = 0xffffffffL
      int.toLong & bitMask
    }

    val intArray = HexReader.readHexFile2IntArray(hexFilePath)

    for(i <- 0 until intArray.length) yield {
      val tmpLong = int2LongBitwise(intArray(i))
      val tmpBigInt = BigInt(tmpLong)
      U(tmpBigInt, 32 bits).asBits.resize(32 bits)
    }
  }

  def main(args: Array[String]):Unit =  {
    val intArray = readHexFile2IntArray("/home/doot/projects/glec/riscv/program.bin")
    for(i <- 0 until intArray.length) {
      val tmp = intArray(i)
      val tmpLong = tmp.toLong
      val mask:Long = 0xffffffffL
      println(tmpLong & mask)
    }
  }
}


