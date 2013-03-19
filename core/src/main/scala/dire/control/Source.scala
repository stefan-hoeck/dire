package dire.control

import dire.{Event, Time, Out}
import collection.mutable.ListBuffer
import scalaz.effect.IO
import scalaz.concurrent.{Actor, Strategy}

private[control] sealed trait EventSource {
  private[control] def node: ChildNode
  def collect(cb: Sink[Unit]): Unit
  def start(): Unit
  def stop(): Unit
}

private[dire] class Source[A](
  starter: Out[A] ⇒ IO[IO[Unit]],
  strategy: Strategy)
    extends EventSource {
  import Source._

  private[this] var onStop: IO[Unit] = IO.ioUnit
  private[this] val collected = new ListBuffer[A]
  private[this] var raw: List[A] = Nil
  private[this] var events: List[Event[A]] = Nil
  private[this] val actor = Actor[SourceE[A]](react)(strategy)

  private[control] object node extends ChildNode {
    protected def doCalc(t: Time) = {
      events = raw map { Event(t, _) }
      events.nonEmpty
    }

    protected def doClean() { raw = Nil; events = Nil }

    override protected def sourceEvents: Boolean = raw.nonEmpty
  }

  private def react(e: SourceE[A]): Unit = e match {
    case Start               ⇒ {
      onStop = starter(a ⇒ IO(actor ! Fired(a))).unsafePerformIO()
    }

    case Stop                ⇒ onStop.unsafePerformIO()
    case Fired(a)            ⇒ collected += a
    case Collect(cb)         ⇒ {
      collected ++= collectEvents
      raw = collected.toList
      collected.clear()
      cb apply ()
    }
  }

  def fired: List[Event[A]] = events
  def collect(cb: Sink[Unit]) { actor ! Collect(cb) }
  def start() { actor ! Start }
  def stop() { actor ! Stop }

  protected def collectEvents: List[A] = Nil
}

private[dire] object Source {
  def apply[A](cb: Out[A] ⇒ IO[IO[Unit]], strategy: Strategy)
    : IO[Source[A]] = IO(new Source[A](cb, strategy))

  def ticks(strategy: Strategy): IO[Source[Unit]] = IO {
    new Source[Unit](_ ⇒ IO(IO.ioUnit), strategy) {
      override protected val collectEvents = List(())
    }
  }

  private sealed trait SourceE[+A]

  private case object Start extends SourceE[Nothing]
  private case object Stop extends SourceE[Nothing]
  private case class Fired[+A](v: A) extends SourceE[A]
  private case class Collect(cb: Sink[Unit]) extends SourceE[Nothing]
}

// vim: set ts=2 sw=2 et:
