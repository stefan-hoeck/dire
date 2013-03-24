package dire.control

import dire._, Change.{NextS, InitialS}
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

  private[dire] def onChange(out: Out[Change[A]]): IO[Unit] = 
    out(last) >> IO {
      node connectChild (
        new ChildNode {
          def doCalc(t: Time) = out(last) as false unsafePerformIO
          def doClean() {}
        }
      )
    }
}

object RawSignal {

  /** Represents a signal that never changes */
  private[dire] case class Const[+A](a: A) extends RawSignal[A] {
    val node = Isolated
    val last = Change(T0, a)
  }

  def const[A](a: ⇒ A): IO[RawSignal[A]] = IO(Const(a))

  private[control] def src[A](s: Source[A]): RawSignal[A] =
    new RawSignal[A]{ def node = s.node; def last = s.last }

  /** Combines two signals to create a new signal that is
    * updated synchronously whenever one or both of the
    * input signals change their state
    */
  private[dire] def sync2[A,B,C]
    (a: RawSignal[A], b: RawSignal[B])
    (ini: InitialS[A,B,C])
    (next: NextS[A,B,C]): IO[RawSignal[C]] = IO {
    new RawSignal[C] {
      private[control] var last: Change[C] = ini(a.last, b.last)

      private[control] val node: ChildNode = new ChildNode {
        protected def doCalc(t: Time) = {
          last = next(a.last, b.last, last)
          last.at == t
        }

        protected def doClean() {}
      }

      a.node connectChild node
      b.node connectChild node
    }
  }
}

// vim: set ts=2 sw=2 et: