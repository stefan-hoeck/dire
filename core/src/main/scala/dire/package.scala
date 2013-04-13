import scalaz.effect.IO

import scalaz._, Scalaz._

package object dire {
  import control._

  /** Time in a descrete reactive setup */
  type Time = Long

  /** The lowest possible value of `Time`*/
  final val T0: Time = 0L

  type Out[-A] = A ⇒ IO[Unit]

  /** Dummy trait to represent pure input signals
    *
    * There are no instances of this trait, so a signal function
    * of type `SIn[A]` can only be run by passing it the empty
    * event stream. This is a bit of a hack. Actually, input signals
    * could be described as SF[Nothing,A] but this led to strange
    * behavior with type inference. 
    */
  sealed trait In

  /** Type alias for an input signal */
  type SIn[+A] = SF[In,A]
}

// vim: set ts=2 sw=2 et:
