package dire

import scalaz._, Scalaz._

/** A (probably temporary) set of helper functions for random
  * number generators.
  *
  * Nothing fancy, this is just needed for a very basich simulation
  * of noise. All code was taken shamelessly from Functional Programming
  * in Scala: http://manning.com/bjarnason/
  */
object Random {
  type RNG[+A] = State[Long,A]

  /** Implementation of a linear congruential generator */
  val lcg: RNG[Int] = State[Long,Int] { s ⇒
    val s2 = (s * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)

    (s2, (s2 >>> 16).asInstanceOf[Int])
  }

  /** A random positive integer */
  val intPos: RNG[Int] = lcg flatMap {
    case Int.MinValue ⇒ intPos
    case x            ⇒ x.abs.η[RNG]
  }

  private final val MaxIntP1 = Int.MaxValue.toDouble + 1D

  /** A random double between 0 (inclusive) and 1 (exclusive) */
  val dbl01: RNG[Double] = intPos map { _ / MaxIntP1 }

  /** Generates random noise (values between 0 and 1)
    * at regular intervals
    */
  def noise(seed: Long, interval: Time = 1000L): SIn[Double] =
    SF ticks interval as dbl01 scanStV seed
}

// vim: set ts=2 sw=2 et:
