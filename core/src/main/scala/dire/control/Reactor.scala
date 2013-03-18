package dire.control

import dire.{Time, T0}
import collection.mutable.ListBuffer
import scalaz.effect.IO
import scalaz.concurrent.{Actor, Strategy}

final private[dire] class Reactor(delay: Time)(implicit S: Strategy) {
  import Reactor._

  private[this] val node = new RootNode
  private[this] val sources = new ListBuffer[EventSource]
  private[this] val readyNodes = new ListBuffer[Node]
  private[this] var state: ReactorState = Embryo
  private[this] var count = 0
  private[this] var time = T0
  private[this] var clockKill: () ⇒ Unit = () ⇒ ()
  private[this] val actor: Actor[ReactorEvent] = Actor(react)
  private[this] val onReady: Sink[Unit] = _ ⇒ actor ! NodeReady

  private[control] def addSource(src: EventSource): IO[Unit] = IO {
    if (state != Embryo) throw new IllegalStateException(
      "Adding event source to Reactor who is not in 'Embryo' state")

    node connectChild src.node
    sources += src
  }

  private[dire] def stop: IO[Unit] = IO { actor ! Shutdown }

  private[dire] def start: IO[Unit] = IO { actor ! Start }

  private def react(ev: ReactorEvent) = (state,ev) match {
    case (Embryo, Start)            ⇒ {
      state = Waiting
      clockKill = Clock(delay, t ⇒ actor ! Tick(t))
      sources foreach { _.start() }
    }

    case (Waiting, Tick(t))         ⇒ {
      state = Collecting
      count = sources.size + 1
      time = t
      sources foreach { _.collect(onReady) }
      actor ! NodeReady
    }

    case (Collecting, NodeReady) ⇒ {
      count -= 1
      if (count == 0) {
        state = Waiting
        node.update(time)
      }
    }

    case (s, Shutdown) if s != Zombie ⇒ {
      state = Zombie
      clockKill apply ()
      sources foreach { _.stop() }
    }

    case _                          ⇒ ()
  }
}

private[dire] object Reactor {
  private sealed trait ReactorState

  private case object Embryo extends ReactorState
  private case object Collecting extends ReactorState
  private case object Waiting extends ReactorState
  private case object Zombie extends ReactorState

  private sealed trait ReactorEvent

  private case object Start extends ReactorEvent
  private case object Shutdown extends ReactorEvent
  private case object NodeReady extends ReactorEvent
  private case class Tick(t: Time) extends ReactorEvent
}

// vim: set ts=2 sw=2 et:
