package dire.control

import dire.{Event, Out, Time}
import scalaz._, Scalaz._, effect._
import scalaz.concurrent.{Actor, Strategy}

class Source[A](
    private[control] val node: Node,
    onStart: Out[A] ⇒ IO[IO[Unit]],
    rawEvents: IORef[List[A]],
    events: IORef[List[Event[A]]])(implicit S: Strategy) {
  import Source._

  private[this] var collected: List[A] = Nil
  private[this] var onStop: IO[Unit] = IO.ioUnit
  private[this] val actor =
    Actor[SourceEvent[A]](react(_).unsafePerformIO)

  private def react(e: SourceEvent[A]): IO[Unit] = e match {
    case Start               ⇒ onStart(a ⇒ IO(actor ! Fired(a))) map { onStop = _ }
    case Stop                ⇒ onStop
    case Fired(a)            ⇒ IO(collected = a :: collected)
    case Collect(cb)         ⇒ doCollect(cb)
  }

  def collect(cb: Out[Option[Node]]): IO[Unit] = IO(actor ! Collect(cb))

  def start: IO[Unit] = IO(actor ! Start)

  def stop: IO[Unit] = IO(actor ! Stop)

  private[control] def fired: IO[List[Event[A]]] = events.read

  private[this] def doCollect(cb: Out[Option[Node]]): IO[Unit] =
    collected match {
      case Nil ⇒ cb(None)
      case as  ⇒ for {
        _ ← rawEvents write as
        _ ← IO(collected = Nil)
        _ ← cb(Some(node))
      } yield ()
    }
    
}

object Source {
  def newSource[A](handler: Out[A] ⇒ IO[IO[Unit]]): IO[Source[A]] = for {
    raw ← IO.newIORef[List[A]](Nil)
    es  ← IO.newIORef[List[Event[A]]](Nil)
    n   ← Node.sourceNode(
            t ⇒ for {
              as ← raw.read
              _  ← es write (as map { Event(t, _) })
              _  ← raw write Nil
            } yield as.nonEmpty,
            es write Nil
          )
  } yield new Source[A](n, handler, raw, es)

  def ticker(interval: Time): IO[Source[Unit]] =
    newSource(o ⇒ Clock(interval, _ ⇒ o(())))

  private[control] sealed trait SourceEvent[+A]

  private[control] case object Start extends SourceEvent[Nothing]

  private[control] case object Stop extends SourceEvent[Nothing]

  private[control] case class Fired[+A](v: A) extends SourceEvent[A]

  private[control] case class Collect(cb: Out[Option[Node]])
    extends SourceEvent[Nothing]
}

// vim: set ts=2 sw=2 et:
