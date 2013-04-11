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

private[control] abstract class DireActor[A](s: Strategy) extends Reactive {
  import DireActor._ 

  protected var stopped = false //true if this actor should ignore events
  private[this] val actor = Actor(react)(s)

  //release all resources
  protected def doStop(): Unit

  //initialize reactive behavior
  protected def doStart(): Unit

  //act upon an event of type a
  protected def doRun(a: A): Unit

  //asynchronously fires and event of type a
  final private[control] def fire(a: A) { actor ! Run(a) }

  //asynchronously starts the actor
  final def start() { actor ! Start }

  //asynchronously stops the actor. Typically the calling thread
  //will wait on the countdown latch. The countdown latch is released
  //once the actor is cleaned up and no more events are to be expeced
  final def stop(cdl: CountDownLatch) { actor ! Stop(cdl) }

  private[this] def react(e: RunE[A]): Unit = e match {
    case Run(a)          ⇒ if (! stopped) doRun(a)
    case Start           ⇒ doStart()
    case Dead(cdl)       ⇒ cdl.countDown()
    case Stop(cdl)       ⇒ {
      stopped = true
      doStop()
      actor ! Dead(cdl) //this will be the very last event the actor receives
    }
  }
}

private object DireActor {
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
  extends DireActor[A](reactor.strategy) {

  private[control] val node = new RootNode
  private[this] var stop: IO[Unit] = IO.ioUnit
  private[control] var last: Change[A] = Change(T0, initial)

  protected def doRun(a: A) {
    reactor fire { t ⇒ 
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
  extends DireActor[Change[A]](sink.strategy getOrElse reactor.strategy) {
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
