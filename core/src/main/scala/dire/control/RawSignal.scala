package dire.control

import dire._
import scalaz._, Scalaz._, effect.IO

/** The inner workings of signals
  *
  * A signal consists of a node that represents its dependencies
  * in the reactive graph and a method to request the latest
  * change that happened in the signal
  */
sealed trait RawSignal[+A] { self ⇒ 
  private[control] def node: Node

  private[control] def last: Change[A]

  private[dire] def map[B](f: A ⇒ B): IO[RawSignal[B]] =
    RawSignal.sync1(this)(c ⇒ f(c.v))((c,_) ⇒ Some(f(c.v)))
}

/** Represents a signal that never changes */
final private[dire] case class Const[+A](a: A) extends RawSignal[A] {
  val node = Isolated
  val last = Change(T0, a)

  override private[dire] def map[B](f: A ⇒ B): IO[RawSignal[B]] =
    IO(Const(f(a)))
}

final private[dire] class RawSource[A](s: RSource[A]) extends RawSignal[A] {
  def node = s.node
  def last = s.last
}

/** A derived signal that updates synchronously when its parent
  * changes
  */
final private[dire] class Sync[A](
  ini: Change[A], next: Change[A] ⇒ Option[Change[A]])
  extends RawSignal[A] {
  private[control] var last: Change[A] = ini

  private[control] val node: ChildNode = Node child { t ⇒ 
    last = next(last) getOrElse last
    //println(s"new value: $last; changed: ${last.at == t}")
    last.at == t
  }
}

object RawSignal {

  def const[A](a: ⇒ A): IO[RawSignal[A]] = IO(Const(a))

  /** Creates a derived signal from an existing one.
    *
    * The new signal is updated synchronously whenever its parent
    * changes.
    */
  private[dire] def sync1[A,B]
    (a: RawSignal[A])
    (ini: Change[A] ⇒ B)
    (next: (Change[A],Change[B]) ⇒ Option[B])
    : IO[RawSignal[B]] = IO {
    val sync = new Sync[B](a.last as ini(a.last),
                           next(a.last, _) map (Change(a.last.at, _)))

    a.node connectChild sync.node
    sync
  }

  /** Combines two signals to create a new signal that is
    * updated synchronously whenever one or both of the
    * input signals change their state
    */
  private[dire] def sync2[A,B,C]
    (a: RawSignal[A], b: RawSignal[B])
    (ini: (Change[A],Change[B]) ⇒ C)
    (next: (Change[A],Change[B],Change[C]) ⇒ Option[C])
    : IO[RawSignal[C]] = IO {
    val sync =
      new Sync[C](Change(a.last.at max b.last.at, ini(a.last, b.last)),
                  next(a.last, b.last, _) map (
                    Change(a.last.at max b.last.at, _)))

    a.node connectChild sync.node
    b.node connectChild sync.node
    sync
  }
}

// vim: set ts=2 sw=2 et:
