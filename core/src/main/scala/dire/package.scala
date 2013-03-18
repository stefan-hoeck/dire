import scalaz.effect.IO

package object dire {
  import control._

  /** Time in a descrete reactive setup */
  type Time = Long

  /** The lowest possible value of `Time`*/
  final val T0: Time = 0L

  type Out[-A] = A ⇒ IO[Unit]

  /** Type alias for a function that creates a
    * stream of events.
    *
    * In dire, you do not work with even streams
    * directly but with stream functions.
    */
  type Events[+A] = Reactor ⇒ IO[RawEvents[A]]

  type EIn[+A] = EF[Any,A]
}

// vim: set ts=2 sw=2 et:
