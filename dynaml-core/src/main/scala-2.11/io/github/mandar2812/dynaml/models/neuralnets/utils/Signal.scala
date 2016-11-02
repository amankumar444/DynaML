package io.github.mandar2812.dynaml.models.neuralnets.utils

/**
  * Created by mandar on 02/11/2016.
  */
sealed trait Signal[T]

case class UnitSignal(direction: String, d: Double) extends Signal[Double]

case class BatchSignal[T <: Traversable[Double]](direction: String, d: T) extends Signal[T]

object Signal {
  final val F = "f"
  final val B = "b"
}

