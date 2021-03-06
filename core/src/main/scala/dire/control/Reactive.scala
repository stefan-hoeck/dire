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
  def source[A](ini: Event[A],
                r: Reactor,
                st: StrategyO = None)
               (cb: Out[A] ⇒ IO[IO[Unit]]): IO[RSource[A]] = for {
    s ← IO(new RSource[A](ini, r, cb, st))
    _ ← r addReactive s
  } yield s

  def sourceNoCb[A](ini: Event[A],
                    r: Reactor,
                    st: StrategyO = None): IO[RSource[A]] =
    source[A](ini, r, st)(_ ⇒ IO(IO.ioUnit))
}

private[control] abstract class DireActor[A](s: Strategy) extends Reactive {
  import DireActor._ 

  protected var active = true //true if this actor should ignore events
  private[this] val actor = Actor(react)(s)

  //release all resources
  protected def doStop(): Unit

  //initialize reactive behavior
  protected def doStart(): Unit

  //act upon an event of type a
  protected def doRun(a: A, active: Boolean): Unit

  //asynchronously fires and event of type a
  final private[control] def fire(a: A) { actor ! Run(a) }

  //asynchronously starts the actor
  final def start() { actor ! Start }

  //asynchronously stops the actor. Typically the calling thread
  //will wait on the countdown latch. The countdown latch is released
  //once the actor is cleaned up and no more events are to be expected
  final def stop(cdl: CountDownLatch) { actor ! Stop(cdl) }

  private[this] def react(e: RunE[A]): Unit = e match {
    case Run(a)          ⇒ doRun(a, active)
    case Start           ⇒ doStart()
    case Dead(cdl)       ⇒ cdl.countDown()
    case Stop(cdl)       ⇒ {
      active = false
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
    initial: Event[A],
    reactor: Reactor,
    setup: Out[A] ⇒ IO[IO[Unit]],
    strategy: StrategyO)
  extends DireActor[A](strategy getOrElse reactor.strategy) {

  private[control] val node = new RootNode
  private[this] var stop: IO[Unit] = IO.ioUnit
  private[control] var last: Event[A] = initial

  protected def doRun(a: A, active: Boolean) = if(active) {
    reactor fire { t ⇒ 
      last = Once(t, a)
      node.update(t)
    }
  }

  protected def doStart() { stop = setup(a ⇒ IO(fire(a))).unsafePerformIO() }

  protected def doStop() = stop.unsafePerformIO
}

// vim: set ts=2 sw=2 et:
