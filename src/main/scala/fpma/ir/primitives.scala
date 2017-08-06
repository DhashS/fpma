package fpma.ir

/**
  * Created by dhash on 7/26/17.
  */
import shapeless.{:+:, ::, CNil, HList, HNil, Inl, Inr}

import scala.reflect.ClassTag
import squants.space.{CubicMeters, Litres, Millilitres, Volume, VolumeUnit}
import shapeless.Generic
import squants.Dimension

import scala.concurrent.duration.Duration
/*object NonNegativeVolume extends Dimension[Volume] {
  def apply = parse _
  override def name = "NonNegativeVolume"
  override def primaryUnit = Volume.primaryUnit
  override def siUnit = Volume.siUnit
  override def units = Volume.units
}
 */

object NonNegativeVolume extends Dimension[Volume] {
  override def name = "NonNegativeVolume"

  override def primaryUnit = Volume.primaryUnit
  override def siUnit = Volume.siUnit
  override def units = Volume.units
}

case class FluidClass(name: String)
case class Fluid[T <: FluidClass](name: String, vol: Volume)

object Measure {
  def apply[T <: FluidClass: ClassTag](
      fluid: Fluid[T],
      volume: Volume): Fluid[T] :: Fluid[T] :: HNil = {
    Fluid[T](fluid.name, fluid.vol - volume) :: Fluid[T](fluid.name, volume) :: HNil
  }
}

class Mixer[A <: FluidClass: ClassTag,
            B <: FluidClass: ClassTag,
            C <: FluidClass: ClassTag]{
  //The List[Fluid] is the other case, when reactants don't combine 100%
  //Type C denotes the desired mix fluid type
  type FluidMix = Fluid[A] :+: (List[Fluid[_]] :: Fluid[C] :: HNil) :+: CNil
  def mix(f1: Fluid[A], f2: Fluid[B], name: String): FluidMix = {
    Inl(Fluid[A](name, f1.vol + f2.vol))
  }
  //TODO support separate FluidClass mixing
}
object Mixer {
  def apply[T <: FluidClass : ClassTag](f1: Fluid[T], f2: Fluid[T], name:String): Fluid[T] = new Mixer[T, T, T].mix(f1, f2, name).head.get
}

class Storage[T <: FluidClass: ClassTag](fluid: Fluid[T], vol: Volume) {
  type StorageTake = (Fluid[T]) :+: (Storage[T] :: Fluid[T] :: HNil) :+: CNil
  def store(fl: Fluid[T], vol: Volume): Storage[T] = {
    new Storage[T](fl, vol)
  }
  def take(volume: Volume): StorageTake = {
    if (volume == vol)
      Inl(new Fluid[T](fluid.name, fluid.vol))
    else
      Inr(
        Inl(
          new Storage[T](fluid, vol - volume) :: new Fluid[T](fluid.name,
                                                              volume) :: HNil))
  }
}
object Storage {
  def apply[T <: FluidClass : ClassTag](fluid: Fluid[T], vol: Volume): Storage[T] = new Storage[T](fluid, vol)
}