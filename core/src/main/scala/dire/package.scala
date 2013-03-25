import scalaz.effect.IO

package object dire {
  import control._

  /** Time in a descrete reactive setup */
  type Time = Long

  /** The lowest possible value of `Time`*/
  final val T0: Time = 0L

  type Out[-A] = A ⇒ IO[Unit]

  /** Type alias for signals */
  type Signal[+A] = Reactor ⇒ IO[RawSignal[A]]

  /** Type alias for event streams
    *
    * In dire, an event stream is a signal
    * of [[dire.Event]]s. Class 'Event' is isomorphic to 'Option'
    * but it is not possible to create instances of 'Event' in
    * client code. This is to make sure that clients do not
    * create event streams that are not well behaved.
    */
  type Events[+A] = Signal[Event[A]]

  type EF[-A,+B] = SF[A,Event[B]]

  type SIn[+A] = SF[Any,A]

  type SOut[-A] = EF[A,Nothing]

  type EIn[+A] = EF[Any,A]
}

// vim: set ts=2 sw=2 et:
