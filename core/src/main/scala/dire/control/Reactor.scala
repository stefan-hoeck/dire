package dire.control

import dire.{Time, T0}
import collection.mutable.{ListBuffer, HashMap ⇒ MMap}
import java.util.concurrent.CountDownLatch
import scalaz.effect.IO
import scalaz.concurrent.{Actor, Strategy}

final private[dire] class Reactor(
  step: Time,
  doKill: () ⇒ Boolean,
  countDownToDeath: CountDownLatch,
  private[dire] val strategy: Strategy) {
  import Reactor._

  private[this] val sourceMap = new MMap[Any,Any]
  private[this] val sources = new ListBuffer[EventSource]
  private[this] var state: ReactorState = Embryo
  private[this] var time = T0
  private[this] val actor: Actor[ReactorEvent] = Actor(react)(strategy)

  private[this] lazy val timer: RawSignal[Time] =
    RawSignal.signal[Time](T0, this)(o ⇒
      IO {
        val kill = Clock(T0, step, o(_).unsafePerformIO)

        IO { kill apply () }
      }
    ).unsafePerformIO

  private[control] def addSource(src: EventSource): IO[Unit] = IO {
    if (state != Embryo) throw new IllegalStateException(
      "Adding event source to Reactor which is not in 'Embryo' state")

    sources += src
  }

  private[control] def run(sink: Sink[Time]) { actor ! Run(sink) }

  private[dire] def start: IO[Unit] = IO { actor ! Start }

  private[dire] def getTimer: IO[RawSignal[Time]] = IO(timer)

  private def react(ev: ReactorEvent) = (state,ev) match {
    case (Active, Run(sink))         ⇒ {
      time += 1L
      sink(time)

      if (doKill()) {
        state = Zombie
        //await shutdown for all sources
        val cdl = new CountDownLatch(sources.size)
        sources foreach { _.stop(cdl) }
        cdl.await()

        //now that sources are shutdown, fire ConfirmDeath. This
        //will be the very last event fired to the actor, so
        //the caller know that it is now safe to shutdown
        //ExecutorServices
        actor ! ConfirmDeath
      }
    }

    case (Embryo, Start)            ⇒ {
      state = Active
      sources foreach { _.start() }
    }

    case (Zombie,ConfirmDeath)      ⇒ { countDownToDeath.countDown() }

    case _                          ⇒ ()
  }
}

private[dire] object Reactor {
  private sealed trait ReactorState

  private case object Embryo extends ReactorState
  private case object Active extends ReactorState
  private case object Zombie extends ReactorState

  private sealed trait ReactorEvent

  private case object Start extends ReactorEvent
  private case object ConfirmDeath extends ReactorEvent
  private case class Run(sink: Sink[Time]) extends ReactorEvent
}

// vim: set ts=2 sw=2 et:
