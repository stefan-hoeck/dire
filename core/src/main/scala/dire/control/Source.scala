package dire.control

import dire._
import collection.mutable.ListBuffer
import java.util.concurrent.CountDownLatch
import scalaz.effect.IO
import scalaz.concurrent.{Actor, Strategy}

private[control] sealed trait EventSource {
  def start(): Unit
  def stop(cdl: CountDownLatch): Unit
}

private[dire] class Source[A](
  initial: A,
  reactor: Reactor,
  starter: Out[A] ⇒ IO[IO[Unit]])
    extends EventSource {
  import Source._

  private[control] val node = new RootNode
  private[this] var onStop: IO[Unit] = IO.ioUnit
  private[this] val actor = Actor[SourceE[A]](react)(reactor.strategy)
  private[control] var last: Change[A] = Change(T0, initial)
  private[this] var stopped = false

  private def react(e: SourceE[A]): Unit = e match {
    case Fired(a)             ⇒ if (! stopped) reactor.run(doFire(a))
    case Stop(cdl)            ⇒ {
      stopped = true
      onStop.unsafePerformIO()
      cdl.countDown()
    }
    case Start                ⇒ 
      onStop = starter(as ⇒ IO(actor ! Fired(as))).unsafePerformIO()
  }

  def start() { actor ! Start }
  def stop(cdl: CountDownLatch) { actor ! Stop(cdl) }

  private[this] def doFire(a: A)(t: Time) {
    last = Change(t, a)
    node.update(t)
  }
}

private[dire] object Source {
  def signalSource[A](initial: ⇒ A, r: Reactor)
                     (cb: Out[A] ⇒ IO[IO[Unit]]): IO[Source[A]] =
    IO(new Source[A](initial, r, cb))

  def eventSource[A](cb: Out[A] ⇒ IO[IO[Unit]])
                    (r: Reactor): IO[Source[Event[A]]] =
    IO(new Source[Event[A]](Never, r, oe ⇒ cb(a ⇒ oe(Once(a)))))

  private sealed trait SourceE[+A]

  private case object Start extends SourceE[Nothing]
  private case class Stop(cdl: CountDownLatch) extends SourceE[Nothing]
  private case class Fired[+A](a: A) extends SourceE[A]
}

// vim: set ts=2 sw=2 et:
