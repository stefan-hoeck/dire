package dire.control

import dire._
import collection.mutable.{ListBuffer, HashMap ⇒ MMap}
import java.util.concurrent.CountDownLatch
import scala.reflect.runtime.universe._
import scalaz.effect.IO
import scalaz.concurrent.{Actor, Strategy}

final private[dire] class Reactor(
  private[dire] val step: Time,
  doKill: () ⇒ Boolean,
  countDownToDeath: CountDownLatch,
  private[dire] val strategy: Strategy) {
  import Reactor._

  //cache for tagged signal functions
  private[this] val cache = new Cache

  //child-actors that need to be stopped when this reactor is stopped
  private[this] val reactives = new ListBuffer[Reactive]

  //this reactor's actual state (Embryo, Active, or Zombie)
  private[this] var state: ReactorState = Embryo

  //abstract time value: This is increased by 1 whenever an event
  //is being processed. It should not be used outside the reactive
  //framework, since it is just a counter
  private[this] var time = T0

  //the actor that controls the whole reactive dependency graph
  private[this] val actor: Actor[ReactorEvent] = Actor(react)(strategy)

  //creates a new source of events
  //must only be called when initializing the reactive graph
  //or (probably) when processing a signal's update event
  private[dire] def source[A]
    (ini: IO[A])
    (cb: Out[A] ⇒ IO[IO[Unit]]): IO[RawSignal[A]] = for {
      s ← RSource(ini, this)(cb)
      _ ← IO(reactives += s)
    } yield new RawSource(s)

  //creates a new anychronous data sink
  //must only be called when initializing the reactive graph
  //or (probably) when processing a signal's update event
  private[dire] def sink[O](snk: DataSink[O])
                           (r: RawSignal[O]): IO[RawSignal[O]] = IO {
    val s = new RSink[O](snk.strategy getOrElse strategy,
                         r.last,
                         snk.out,
                         snk.cleanSink)
    reactives += s

    val n = new ChildNode {
      def doCalc(t: Time) = { s.output(r.last); false }
      def doClean() {}
    }

    r.node connectChild n

    r
  }

  private[dire] def trans[A,B](f: A ⇒ IO[B])(in: RawSignal[Event[A]])
    : IO[RawSignal[Event[B]]] = for {
      v  ← Var[Event[B]](Never, strategy)

      si = DataSink.create[Event[A]](
        _.fold(f(_) flatMap { b ⇒ IO(v.fire(Once(b))) }, IO.ioUnit)
      )

      so = Var.VarSource[Event[B]]
      _  ← sink(si)(in)
      s  ← source[Event[B]](so ini v)(so cb  v)
      _  ← IO(reactives += v)
    } yield s

  private[dire] def timeSignal: IO[RawSignal[Time]] =
    cached[Event[Nothing],Time](
      (_,_) ⇒ source[Time](IO(T0))(Clock(T0, step, _)), "DireTime")(
      Const(Never))

  //lookup a cached RawSignal for the given input and output type. 
  private[dire] def cached[In:TypeTag,Out:TypeTag]
    (sig: (RawSignal[In],Reactor) ⇒ IO[RawSignal[Out]], key: Any)
    (in: RawSignal[In]): IO[RawSignal[Out]] = {
    def io: IO[(RawSignal[In],RawSignal[Out])] = sig(in,this) map { (in,_) }

    cache(io, key) map { _._2 }
  }

  //loops the output of a signal function asynchronuously back to
  //its input
  private[dire] def loop[A]
    (ini: ⇒ A)
    (f: (RawSignal[A], Reactor) ⇒ IO[RawSignal[A]]): IO[RawSignal[A]] = for {
    s   ← IO(new RSource[A](ini, this, _ ⇒ IO(IO.ioUnit)))
    _   ← IO(reactives += s)
    r   ← IO(new RawSource(s))
    re  ← f(r, this)
    n   = new ChildNode {
            def doCalc(t: Time) = { s.fire(re.last.v); false }
            def doClean() {}
          }
    _   ← IO(re.node.connectChild(n))
  } yield re

  //called from child actors in signal sources when they want
  //to notify the reactor about an event that happened.
  //`sink` is a callback that will update the reactive graph
  //when invoked
  private[control] def run(sink: Sink[Time]) { actor ! Run(sink) }

  //starts the reactor and all its signal sources
  private[dire] def start: IO[Unit] = IO { actor ! Start }

  private def react(ev: ReactorEvent) = (state,ev) match {
    //handles an event
    case (Active, Run(update))         ⇒ {
      time += 1L //increase time
      update(time) //updates the reaktive graph

      //check wether we have to stop the reactor
      if (doKill()) {
        //ignore all subsequent events
        state = Zombie

        //await shutdown for all sources
        await(reactives.size, cdl ⇒ reactives foreach { _.stop(cdl) })

        //now that all sources are shutdown, fire ConfirmDeath. This
        //will be the very last event fired to the actor
        actor ! ConfirmDeath
      }
    }

    //starts the reactor and all reactive sources and sinks
    case (Embryo, Start)            ⇒ {
      state = Active
      reactives foreach { _.start() }
    }

    //its safe now to release the CountDownLatch since we will
    //never receive another event
    case (Zombie,ConfirmDeath)      ⇒ { countDownToDeath.countDown() }

    //ignore all other events
    case _                          ⇒ ()
  }
}

private object Reactor {
  sealed trait ReactorState

  case object Embryo extends ReactorState
  case object Active extends ReactorState
  case object Zombie extends ReactorState

  sealed trait ReactorEvent

  case object Start extends ReactorEvent
  case object ConfirmDeath extends ReactorEvent
  case class Run(sink: Sink[Time]) extends ReactorEvent
}

// vim: set ts=2 sw=2 et:
