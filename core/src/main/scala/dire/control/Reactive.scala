package dire.control

import dire._
import collection.mutable.ListBuffer
import java.util.concurrent.CountDownLatch
import scalaz.effect.IO
import scalaz.concurrent.{Actor, Strategy}

private[control] sealed trait Reactive {
  def start(): Unit
  def stop(cdl: CountDownLatch): Unit
}

private[control] class RSource[A](
  initial: A,
  reactor: Reactor,
  starter: Out[A] ⇒ IO[IO[Unit]])
    extends Reactive {
  import RSource._

  private[control] val node = new RootNode(() ⇒ reactor.destroy(this))
  private[this] var onStop: IO[Unit] = IO.ioUnit
  private[this] val actor = Actor[SourceE[A]](react)(reactor.strategy)
  private[control] var last: Change[A] = Change(T0, initial)
  private[this] var stopped = false

  private[control] def fire(a: A) { actor ! Fired(a) }

  private def react(e: SourceE[A]): Unit = e match {
    case Fired(a)             ⇒ if (! stopped) reactor.run(doFire(a))

    case Stop(cdl)            ⇒ {
      stopped = true
      onStop.unsafePerformIO()
      actor ! Dead(cdl)
    }

    case Start                ⇒ 
      onStop = starter(a ⇒ IO(fire(a))).unsafePerformIO()

    case Dead(cdl)            ⇒ cdl.countDown()
  }

  def start() { actor ! Start }
  def stop(cdl: CountDownLatch) { actor ! Stop(cdl) }

  private[this] def doFire(a: A)(t: Time) {
    last = Change(t, a)
    node.update(t)
  }
}

private object RSource {
  def apply[A](ini: IO[A], r: Reactor)(cb: Out[A] ⇒ IO[IO[Unit]])
    :IO[RSource[A]] = for {
      a   ← ini
      res ← IO(new RSource[A](a, r, cb))
    } yield res

  sealed trait SourceE[+A]

  case object Start extends SourceE[Nothing]
  case class Stop(cdl: CountDownLatch) extends SourceE[Nothing]
  case class Dead(cdl: CountDownLatch) extends SourceE[Nothing]
  case class Fired[+A](a: A) extends SourceE[A]
}

private[control] class RSink[A](
  s: Strategy, ini: Change[A], out: Out[Change[A]], cleanup: IO[Unit])
    extends Reactive {
  import RSink._

  private[this] val actor = Actor[SinkE[A]](react)(s)
  private[this] var stopped = false

  private[control] def output(c: Change[A]) { actor ! Changed(c) }

  private def react(e: SinkE[A]): Unit = e match {
    case Changed(c)             ⇒ if (! stopped) out(c).unsafePerformIO

    case Stop(cdl)            ⇒ {
      stopped = true
      cleanup.unsafePerformIO()
      actor ! Dead(cdl)
    }

    case Start                ⇒ { out(ini).unsafePerformIO }

    case Dead(cdl)            ⇒ cdl.countDown()
  }

  def start() = { actor ! Start }
  def stop(cdl: CountDownLatch) = { actor ! Stop(cdl) }
}

private object RSink {

  sealed trait SinkE[+A]

  case object Start extends SinkE[Nothing]
  case class Stop(cdl: CountDownLatch) extends SinkE[Nothing]
  case class Dead(cdl: CountDownLatch) extends SinkE[Nothing]
  case class Changed[+A](c: Change[A]) extends SinkE[A]
}

// vim: set ts=2 sw=2 et:
