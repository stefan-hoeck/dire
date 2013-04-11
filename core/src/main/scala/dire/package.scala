import scalaz.effect.IO

import scalaz._, Scalaz._

package object dire {
  import control._

  /** Time in a descrete reactive setup */
  type Time = Long

  /** The lowest possible value of `Time`*/
  final val T0: Time = 0L

  type Out[-A] = A ⇒ IO[Unit]

//  /** Type alias for a signal function from one event stream
//    * to another one */
//  type EF[-A,+B] = RF[A,B,Event,Event]
//
//  /** Type alias for an input event stream */
//  type EIn[+A] = EF[⊥,A]
//
//  /** Type alias for a signal function from an event stream
//    * to a signal */
//  type ESF[-A,+B] = RF[A,B,Event,Id]
//
//  /** Type alias for a signal function from one signal
//    * to another one */
//  type SF[-A,+B] = RF[A,B,Id,Id]
//
//  /** Type alias for a signal function from a signal
//    * to an event stream */
//  type SEF[-A,+B] = RF[A,B,Id,Event]
//
//  /** Type alias for an input signal */
//  type SIn[+A] = ESF[⊥,A]
//
//  object SF extends RFFunctions
}

// vim: set ts=2 sw=2 et:
