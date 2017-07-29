/**
  * Created by dhash on 7/26/17.
  */
import fpma.ir._
import squants.space._


object FPMA {
  def main(args: Array[String]): Unit = {
    val mixable = FluidClass("non-reactive")
    val m = Fluid[mixable.type]("hello", Millilitres(5))

    val nfl = Measure(m, Millilitres(10))
    println(nfl)
  }
}
