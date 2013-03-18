
import scalaz.effect.IO

package object dire {
  /** Time in a descrete reactive setup */
  type Time = Long

  /** The lowest possible value of `Time`*/
  final val T0: Time = 0L

  type Out[-A] = A ⇒ IO[Unit]
}

// vim: set ts=2 sw=2 et:
