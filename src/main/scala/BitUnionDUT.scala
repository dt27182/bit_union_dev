import Chisel._

class BitUnionDUT extends Module {
  val io = new Bundle {
    val headOut = UInt(OUTPUT, width = 3)
    val bodyOut = UInt(OUTPUT, width = 4)
  }
  val headFlit = Wire(new HeadFlit())
  headFlit.bits := UInt(3)
  io.headOut := Flit.head(headFlit).asHead().bits

  val bodyFlit = Wire(new BodyFlit())
  bodyFlit.bits := UInt(10)
  io.bodyOut := Flit.body(bodyFlit).asBody().bits
}

class BitUnionTests(c: BitUnionDUT) extends Tester(c) {
  expect(c.io.headOut, 3)
  expect(c.io.bodyOut, 10)
}

object Hi {
  def main(args: Array[String]): Unit = {
    chiselMainTest(Array("--test", "--compile", "--genHarness", "--backend", "c"), () => Module(new BitUnionDUT())) {(c) => new BitUnionTests(c)}
  }
}
