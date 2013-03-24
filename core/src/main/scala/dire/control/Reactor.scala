package dire.control

import dire.{Time, T0, SourceSignal}
import collection.mutable.{ListBuffer, HashMap ⇒ MMap}
import java.util.concurrent.CountDownLatch
import scala.reflect.runtime.universe._
import scalaz.effect.IO
import scalaz.concurrent.{Actor, Strategy}

final private[dire] class Reactor(
  doKill: () ⇒ Boolean,
  countDownToDeath: CountDownLatch,
  private[dire] val strategy: Strategy) {
  import Reactor._

  private[this] val cache = new ListBuffer[(Type,Type,Any,Any)]
  private[this] val sources = new ListBuffer[EventSource]
  private[this] var state: ReactorState = Embryo
  private[this] var time = T0
  private[this] val actor: Actor[ReactorEvent] = Actor(react)(strategy)

  private[dire] def sourceSignal[S,V](src: S)
                                     (implicit E: SourceSignal[S,V])
                                     : IO[RawSignal[V]] = IO {
    if (state != Embryo) throw new IllegalStateException(
      "Adding event source to Reactor which is not in 'Embryo' state")

    val s = new Source[V](E ini src unsafePerformIO, this, E cb src)
    sources += s

    RawSignal src s
  }

  private[dire] def cached[In:TypeTag,Out:TypeTag](
    sig: Reactor ⇒ IO[RawSignal[Out]], tag: Any): IO[RawSignal[Out]] = {
    val in = typeOf[In]
    val out = typeOf[Out]

    //SF[Any,Nothing] should match SF[Int,Int] but not vice verca
    cache find { case (i,o,t,_) ⇒ (i <:< in) &&
                                  (out <:< o) &&
                                  (t == tag) } match {
      case None    ⇒ for {
                       res ← sig(this)
                       _   ← IO(cache += ((in, out, tag, res)))
                     } yield res
      case Some(t) ⇒ IO(t._4.asInstanceOf[RawSignal[Out]])
    }
  }

  private[control] def run(sink: Sink[Time]) { actor ! Run(sink) }

  private[dire] def start: IO[Unit] = IO { actor ! Start }

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
