package dire.control

import dire._
import collection.mutable.ListBuffer
import java.util.concurrent.CountDownLatch
import scala.reflect.runtime.universe._
import scalaz.effect.IO
import scalaz.syntax.monad._
import scalaz.concurrent.{Actor, Strategy}

final private[dire] class Reactor(
    private[dire] val step: Time,
    doKill: () ⇒ Boolean,
    countDownToDeath: CountDownLatch,
    private[dire] val strategy: Strategy)
  extends DireActor[Sink[Time]](strategy) {

  //cache for reusable reactives
  private[this] val cache = new Cache

  //child-actors that need to be stopped when this reactor is stopped
  private[this] val reactives = new ListBuffer[Reactive]

  //abstract time value: This is increased by 1 whenever an event
  //is being processed. It should not be used outside the reactive
  //framework, since it is just a counter
  private[this] var time = T0

  //connect a reactive with this Reactor
  //the connected reactive's lifetime will depend on the lifetime
  //of this Reactor. It will be started, when this is started,
  //and it will be stopped when this is stopped
  private[dire] def addReactive(r: Reactive): IO[Unit] = IO(reactives += r)

  //creates a new source of events
  //must only be called when initializing the reactive graph
  //or (probably) when processing a signal's update event
  private[dire] def source[A]
    (ini: IO[Event[A]])
    (cb: Out[A] ⇒ IO[IO[Unit]]): IO[RawSignal[A]] =
      ini >>= { Reactive.source(_, this)(cb) } >>= RawSource.apply

  //creates a new anychronous data sink
  //must only be called when initializing the reactive graph
  //or (probably) when processing a signal's update event
  //sinks can be cached, therefor the optional key plus typetag
  private[dire] def sink[O](
    out: Out[Event[O]],
    clean: IO[Unit],
    st: Option[Strategy],
    key: Option[(Any,TypeTag[O])])
    (r: RawSignal[O]): IO[Unit] = {
      def tag(implicit T:TypeTag[O]) = implicitly[TypeTag[Var[Event[O]]]]

      val newVar = Var.forReactor(r.last, this, st)
      val getVar = key.fold(newVar){ case(k,t) ⇒ cache(newVar, k)(tag(t)) }

      def connect(v: Var[Event[O]]) = IO {
        r.node connectChild Node.child{ _ ⇒ v.set(r.last); false }
        v.addListener(out)
      }

      getVar flatMap connect
    }

  private[dire] def trans[A,B]
    (f: A ⇒ IO[B], s: Option[Strategy])
    (in: RawSignal[A]): IO[RawSignal[B]] = for {
      v ← Var.forReactor[Event[IO[B]]](in.last map f, this, s)
      s ← Reactive.sourceNoCb[B](Never, this, s)
      r ← RawSource(s)
      _ ← IO {
            in.node.connectChild(Node.child{_ ⇒ v.set(in.last map f); false})
            v.addListener(_.fold(_ flatMap { b ⇒ IO(s.fire(b)) }, IO.ioUnit))
          }
    } yield r

  private[dire] def timeSignal: IO[RawSignal[Time]] =
    cached[Nothing,Time](
      (_,_) ⇒ source[Time](IO(Once(T0,T0)))(Clock(T0, step, _)), "DireTime")(
      RawSignal.empty)

  //lookup a cached RawSignal for the given input and output type. 
  private[dire] def cached[In:TypeTag,Out:TypeTag]
    (sig: (RawSignal[In],Reactor) ⇒ IO[RawSignal[Out]], key: Any)
    (in: RawSignal[In]): IO[RawSignal[Out]] =
    cache(sig(in,this) map { (in,_) }, key) map { _._2 }

  //loops the output of a signal function asynchronuously back to
  //its input
  private[dire] def loop[A]
    (f: (RawSignal[A], Reactor) ⇒ IO[RawSignal[A]]): IO[RawSignal[A]] = for {
    s   ← Reactive.sourceNoCb[A](Never, this)
    r   ← RawSource(s)
    re  ← f(r, this)
    v   ← Var.forReactor(re.last, this, Some(Strategy.Sequential))
    _   ← IO {
            re.node connectChild Node.child { _ ⇒ 
              re.last.fold(_ ⇒ v.set(re.last), ())
              false
            }

            v addListener { _.fold(a ⇒ IO(s.fire(a)), IO.ioUnit) }
          }
  } yield re

  protected def doRun(update: Sink[Time], act: Boolean) = if (act) {
    time += 1L //increase time
    update(time) //updates the reaktive graph

    //check wether we have to stop the reactor
    if (doKill()) {
      active = false //ignore all subsequent events
      stop(countDownToDeath) //fire Stop event
    }
  }

  protected def doStart() { reactives foreach { _.start() } }

  protected def doStop() {
    await(reactives.size, cdl ⇒ reactives foreach { _.stop(cdl) })
  }
}

// vim: set ts=2 sw=2 et:
