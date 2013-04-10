import scalaz.effect.IO

import scalaz._, Scalaz._

package object dire {
  import control._

  /** Time in a descrete reactive setup */
  type Time = Long

  /** The lowest possible value of `Time`*/
  final val T0: Time = 0L

  type Out[-A] = A ⇒ IO[Unit]

  type EF[-A,+B] = SfT[A,B,Event,Event]

  type EIn[+A] = SEF[Any,A]

  type ESF[-A,+B] = SfT[A,B,Event,Id]

  type SF[-A,+B] = SfT[A,B,Id,Id]

  type SEF[-A,+B] = SfT[A,B,Id,Event]

  type SIn[+A] = SF[Any,A]

  /** The most general function needed to calculate the
    * latest change of a signal from two input signals.
    *
    * All primitive functions that create a new signal from
    * two input signals that is synchronously updated
    * can be expressed using this type and type 'Initial2'.
    */
  private[dire] type Next2[-A,-B,C,F[+_]] = 
    (Change[A], Change[B], Change[F[C]]) ⇒ F[C]

  /** The most general function needed to calculate the
    * initial value of a signal from two input signals.
    *
    * All primitive functions that create a new signal from
    * two input signals that is synchronously updated
    * can be expressed using this type and type 'Next1'.
    */
  private[dire] type Initial2[-A,-B,C,F[+_]] = 
    (Change[A], Change[B]) ⇒ F[C]

  /** The most general function needed to calculate the
    * latest change of a signal from one input signal.
    *
    * All primitive functions that create a new signal from
    * one input signal that is synchronously updated
    * can be expressed using this type and type 'Initial1'.
    */
  private[dire] type Next1[-A,B,F[+_]] = (Change[A], Change[F[B]]) ⇒ F[B]

  /** The most general function needed to calculate the
    * initial value of a signal from one input signal.
    *
    * All primitive functions that create a new signal from
    * one input signal that is synchronously updated
    * can be expressed using this type and type 'Next1'.
    */
  private[dire] type Initial1[-A,+B,F[+_]] = Change[A] ⇒ F[B]
}

// vim: set ts=2 sw=2 et:
