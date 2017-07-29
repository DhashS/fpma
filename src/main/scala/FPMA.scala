/**
  * Created by dhash on 7/26/17.
  */
import fpma.ir._
import squants.space._


object FPMA {
  def main(args: Array[String]): Unit = {
    val mixable = FluidClass("non-reactive")
    val m = Fluid[mixable.type]("hello", Millilitres(5))
    val q = Fluid[mixable.type]("world", Millilitres(10))
    val mx = new Mixer[mixable.type, mixable.type, mixable.type]()

    val to = mx.mix(m, q, "together")
    val nfl = Measure(to.head.get, Millilitres(10))
    val store = Storage(nfl.head, nfl.head.vol)
    println(store)
  }
}
