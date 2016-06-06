//////////////////////////////////////////////////////////////////////////
// Open System-on-a-Chip (OpenSoC) is Copyright (c) 2014,               //
// The Regents of the University of California, through Lawrence        //
// Berkeley National Laboratory (subject to receipt of any required     //
// approvals from the U.S. Dept. of Energy).  All rights reserved.      //
//                                                                      //
// Portions may be copyrighted by others, as may be noted in specific   //
// copyright notices within specific files.                             //
//                                                                      //
// AUTHOR: J. Bachan, F. Fatollahi-Fard                                 //
//////////////////////////////////////////////////////////////////////////

import Chisel._

class HeadFlit() extends Bundle {
  val bits = UInt(width=3)
}

class BodyFlit() extends Bundle {
  val bits = UInt(width=4)
}

class Flit() extends Bundle {
  val union   = new BitUnion(Map("Head" -> Wire(new HeadFlit()), "Body" -> Wire(new BodyFlit())))
  val x       = Chisel.UInt(width = union.width)

  def asHead(dummy: Int = 0) : HeadFlit = union.unpack[HeadFlit]("Head", x)
  def asBody(dummy: Int = 0) : BodyFlit = union.unpack[BodyFlit]("Body", x)

  def whenHead(block: HeadFlit => Unit) { union.whenTag[HeadFlit]("Head", x)(block) }
  def whenBody(block: BodyFlit => Unit) { union.whenTag[BodyFlit]("Body", x)(block) }

  def isHead(dummy: Int = 0) : Bool = union.tagEquals("Head", x)
  def isBody(dummy: Int = 0) : Bool = union.tagEquals("Body", x)

  override def cloneType = { new Flit().asInstanceOf[this.type] }
  // override def width : Int = {x.width}
}

object Flit {
  def head(h: HeadFlit) : Flit = {
    val f = Wire(new Flit())
    f.x := f.union.pack("Head", h)
    f
  }

  def body(b: BodyFlit) : Flit = {
    val f = Wire(new Flit())
    f.x := f.union.pack("Body", b)
    f
  }

  def fromBits(n: UInt) : Flit = {
    val f = Wire(new Flit())
    f.x := n
    f
  }
}

class BitUnion(val tag2data: Map[String, Chisel.Data]) {
  
  val codeWidth = log2Up(tag2data.size)
  val tailWidth = tag2data.values.map(_.toBits.getWidth).foldLeft(0)(math.max)
  val tag2code = {
    val s = tag2data.keys.toArray.sorted
    Map(s zip s.indices:_*)
  }
  
  val width : Int = codeWidth + tailWidth
  
  def tagEquals(tag: String, x: Chisel.Bits) : Chisel.Bool = {
    val code = x.apply(0, codeWidth-1)
    code === Chisel.UInt(tag2code(tag))
  }

  def pack[T <: Chisel.Data](tag: String, x: T) : Chisel.Bits = {
    // val fred = Vec(this.width, UInt(width = 1) )

    // // fred(0, codeWidth - 1).toBits := UInt(tad2code(tag), width = codeWidth)
    val code = UInt(tag2code(tag), width = codeWidth)
    // for (i <- 0 until codeWidth) {
    //   fred(i) := code(i)
    // }

    val xb = x.toBits
    // // println("xb getWidth: " + xb.getWidth + "\twidth: " + xb.width)
    // for (i <- 0 until xb.getWidth) {
    //   fred(codeWidth + i) := xb(i)
    // }

    // println("code width: " + codeWidth + "\txb getWidth: " + xb.getWidth + "\tthis.width: " + this.width)
    val zerosWidth = this.width - (codeWidth + xb.getWidth)
    // for (i <- (codeWidth + xb.getWidth) until this.width) {
    //   fred(i) := UInt(0)
    // }

    // fred.toBits
    if (zerosWidth == 0) {
      Cat(xb, code)
    } else {
      val zeros = UInt(0, width = zerosWidth)
      Cat(zeros, xb, code)
    }
  }

  def unpack[T <: Chisel.Data](tag: String, x: Chisel.Bits) : T = {
    val data = Wire(tag2data(tag).cloneType)
    // println("data getWidth: " + data.toBits.getWidth + "\twidth: " + data.width)
    data.fromBits(x.apply(codeWidth+data.toBits.getWidth-1, codeWidth)).asInstanceOf[T]
  }
  
  def whenTag[T <: Chisel.Data](tag: String, x: Chisel.Bits) (block: T => Unit) = {
    Chisel.when(tagEquals(tag, x)) { block(unpack[T](tag, x)) }
  }
}
