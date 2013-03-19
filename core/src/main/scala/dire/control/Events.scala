package dire.control

import dire.{Event, Out, Time}
import scalaz._, Scalaz._, effect.IO

sealed trait RawEvents[+A] { self ⇒ 
  private[dire] def node: Node

  private[control] def fired: List[Event[A]]

  private[dire] def onEvent(out: Out[A]): IO[Unit] = IO {
    node connectChild (
      new ChildNode {
        def doCalc(t: Time) =
          fired foldMap { e ⇒ out(e.v) } as false unsafePerformIO
        def doClean() {}
      }
    )
  }

  private[dire] def filterIO(p: A ⇒ Boolean): IO[RawEvents[A]] =
    toE[A] { _ collect { case e@Event(t,a) if p(a) ⇒ e } }

  private[dire] def mergeIO[B>:A](that: RawEvents[B]): IO[RawEvents[B]] =
    toE[B] { _ ::: that.fired }

  private[dire] def mapEventIO[B](f: Event[A] ⇒ B): IO[RawEvents[B]] =
    toE[B] { _ map { case e@Event(t,a) ⇒ Event(t, f(e)) } }

  private[dire] def toE[B](f: List[Event[A]] ⇒ List[Event[B]])
    : IO[RawEvents[B]] = IO {
      val res = new RawEvents[B] {
        private[this] var es: List[Event[B]] = Nil
        private[dire] val node: ChildNode = new ChildNode {
          protected def doCalc(t: Time) = { es = f(self.fired); es.nonEmpty }
          protected def doClean() { es = Nil }
        }

        self.node connectChild node

        private[dire] def fired = es
      }

      res
    }
}

object RawEvents {
  case object Never extends RawEvents[Nothing] {
    val node = Isolated
    val fired = Nil
  }

  def src[A](callback: Out[A] ⇒ IO[IO[Unit]])(r: Reactor)
    : IO[RawEvents[A]] = for {
      s   ← Source(callback, r.strategy)
      _   ← r addSource s
      res = new RawEvents[A]{ def node = s.node; def fired = s.fired }
    } yield res

  def ticks(r: Reactor): IO[RawEvents[Unit]] = for {
    s   ← Source ticks r.strategy
    _   ← r addSource s
    res = new RawEvents[Unit]{ def node = s.node; def fired = s.fired }
  } yield res

  def timer(step: Time)(r: Reactor): IO[RawEvents[Time]] =
    src[Time](o ⇒ IO {
      val kill = Clock(step, o(_).unsafePerformIO)

      IO(kill apply ())
    })(r)
}

// vim: set ts=2 sw=2 et:
