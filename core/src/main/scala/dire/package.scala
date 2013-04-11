import scalaz.effect.IO

import scalaz._, Scalaz._

package object dire {
  import control._

  /** Time in a descrete reactive setup */
  type Time = Long

  /** The lowest possible value of `Time`*/
  final val T0: Time = 0L

  type Out[-A] = A ⇒ IO[Unit]

  /** Type alias for an input signal */
  type SIn[+A] = SF[⊥,A]
}

// vim: set ts=2 sw=2 et:
