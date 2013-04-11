package dire.control

import dire._
import collection.mutable.ListBuffer
import java.util.concurrent.CountDownLatch
import scalaz.effect.IO
import scalaz.concurrent.{Actor, Strategy}

private[control] trait Reactive {
  def start(): Unit
  def stop(cdl: CountDownLatch): Unit
}

private[control] object Reactive {
  def source[A](ini: IO[A], r: Reactor)
               (cb: Out[A] ⇒ IO[IO[Unit]]):IO[RSource[A]] =
    ini flatMap { a ⇒ IO(new RSource[A](a, r, cb)) }

  def sink[A](sink: DataSink[A], r: Reactor)(raw: RawSignal[A])
    : IO[RSink[A]] = IO(new RSink[A](sink, r, raw))
}

private[control] abstract class WithActor[A](
    val reactor: Reactor,
    s: Option[Strategy]) 
  extends Reactive {
  import WithActor._ 

  private[this] var stopped = false
  private[this] val actor = Actor(react)(s getOrElse reactor.strategy)

  private[control] def node: Node

  protected def doStop(): Unit

  protected def doStart(): Unit

  protected def doRun(a: A): Unit

  final private[control] def fire(a: A) { actor ! Run(a) }

  final def start() { actor ! Start }

  final def stop(cdl: CountDownLatch) { actor ! Stop(cdl) }

  private[this] def react(e: RunE[A]): Unit = e match {
    case Run(a)          ⇒ if (! stopped) doRun(a)

    case Stop(cdl)       ⇒ {
      stopped = true
      doStop()
      actor ! Dead(cdl)
    }

    case Start           ⇒ doStart()

    case Dead(cdl)       ⇒ cdl.countDown()
  }
}

private object WithActor {
  sealed trait RunE[+A]

  case object Start extends RunE[Nothing]
  case class Stop(cdl: CountDownLatch) extends RunE[Nothing]
  case class Dead(cdl: CountDownLatch) extends RunE[Nothing]
  case class Run[+A](a: A) extends RunE[A]
}

//A source of events
final private[control] class RSource[A](
    initial: A,
    reactor: Reactor,
    setup: Out[A] ⇒ IO[IO[Unit]])
  extends WithActor[A](reactor, None) {

  private[control] val node = new RootNode
  private[this] var stop: IO[Unit] = IO.ioUnit
  private[control] var last: Change[A] = Change(T0, initial)

  protected def doRun(a: A) {
    reactor.run { t ⇒ 
      last = Change(t, a)
      node.update(t)
    }
  }

  protected def doStart() { stop = setup(a ⇒ IO(fire(a))).unsafePerformIO() }

  protected def doStop() = stop.unsafePerformIO
}

//A data sink
private[control] class RSink[A](
    sink: DataSink[A],
    reactor: Reactor,
    raw: RawSignal[A])
  extends WithActor[Change[A]](reactor, sink.strategy) {
  private[control] val node = new ChildNode {
    def doCalc(t: Time) = { fire(raw.last); false }
    def doClean() {}
  }

  raw.node.connectChild(node)

  protected def doRun(c: Change[A]) { sink.out(c).unsafePerformIO() }
  protected def doStop() { sink.cleanSink.unsafePerformIO() }
  protected def doStart() { doRun(raw.last) }
}

// vim: set ts=2 sw=2 et:
