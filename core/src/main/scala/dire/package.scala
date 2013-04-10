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

  type EIn[+A] = EF[⊥,A]

  type ESF[-A,+B] = SfT[A,B,Event,Id]

  type SF[-A,+B] = SfT[A,B,Id,Id]

  type SEF[-A,+B] = SfT[A,B,Id,Event]

  type SIn[+A] = ESF[⊥,A]
}

// vim: set ts=2 sw=2 et:
