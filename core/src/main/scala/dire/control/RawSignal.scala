package dire.control

import dire._
import scalaz._, Scalaz._, effect.IO

/** The inner workings of signals
  *
  * A signal consists of a node that represents its dependencies
  * in the reactive graph and a method to request the latest
  * event that happened in the signal
  */
sealed trait RawSignal[+A] { self ⇒ 
  private[control] def node: Node

  private[control] def last: Event[A]

  private[dire] def map[B](f: A ⇒ B): IO[RawSignal[B]] =
    RawSignal.sync1(this)(_ map f)((c,_) ⇒ c map f)
}

/** Represents a signal that never changes */
final private[dire] case class Const[+A](last: Event[A]) extends RawSignal[A] {
  val node = Isolated

  override private[dire] def map[B](f: A ⇒ B): IO[RawSignal[B]] =
    IO(Const(last map f))
}

final private[dire] class RawSource[A] private(s: RSource[A])
   extends RawSignal[A] {
  def node = s.node
  def last = s.last
}

private[dire] object RawSource {
  def apply[A](s: RSource[A]): IO[RawSource[A]] = IO(new RawSource(s))
}

/** A derived signal that updates synchronously when its parent
  * changes
  */
final private[dire] class Sync[A](ini: Event[A], next: Event[A] ⇒ Event[A])
  extends RawSignal[A] {
  private[control] var last: Event[A] = ini

  private[control] val node: ChildNode = Node child { t ⇒ 
    last = next(last) orElse last
    //println(s"new value: $last; changed: ${last.at == t}")
    last.at == t
  }
}

private[dire] object RawSignal {

  private[dire] val empty: RawSignal[Nothing] = Const(Never)

  def const[A](a: ⇒ A): IO[RawSignal[A]] = IO(Const(Once(T0,a)))

  def never[A]: IO[RawSignal[A]] = IO(empty)

  /** Creates a derived signal from an existing one.
    *
    * The new signal is updated synchronously whenever its parent
    * changes.
    */
  private[dire] def sync1[A,B]
    (a: RawSignal[A])
    (ini: Event[A] ⇒ Event[B])
    (next: (Event[A],Event[B]) ⇒ Event[B])
    : IO[RawSignal[B]] = IO {
    val sync = new Sync[B](
      ini(a.last) orAt a.last.at,
      eb ⇒ next(a.last, eb) orAt a.last.at
    )

    a.node connectChild sync.node
    sync
  }

  /** Combines two signals to create a new signal that is
    * updated synchronously whenever one or both of the
    * input signals change their state
    */
  private[dire] def sync2[A,B,C]
    (a: RawSignal[A], b: RawSignal[B])
    (ini: (Event[A],Event[B]) ⇒ Event[C])
    (next: (Event[A],Event[B],Event[C]) ⇒ Event[C])
    : IO[RawSignal[C]] = IO {
    val sync = new Sync[C](
      ini(a.last, b.last) orAt (a.last.at max b.last.at),
      eb ⇒ next(a.last, b.last, eb) orAt (a.last.at max b.last.at)
    )

    a.node connectChild sync.node
    b.node connectChild sync.node
    sync
  }
}

// vim: set ts=2 sw=2 et:
