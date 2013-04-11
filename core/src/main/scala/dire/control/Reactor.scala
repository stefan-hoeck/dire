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

  //creates a new source of events
  //must only be called when initializing the reactive graph
  //or (probably) when processing a signal's update event
  private[dire] def source[A]
    (ini: IO[A])
    (cb: Out[A] ⇒ IO[IO[Unit]]): IO[RawSignal[A]] = for {
      s ← Reactive.source(ini, this)(cb)
      _ ← IO(reactives += s)
    } yield new RawSource(s)

  //creates a new anychronous data sink
  //must only be called when initializing the reactive graph
  //or (probably) when processing a signal's update event
  private[dire] def sink[O](
    out: Out[Change[O]],
    clean: IO[Unit],
    st: Option[Strategy],
    key: Option[(Any,TypeTag[O])])
    (r: RawSignal[O]): IO[Unit] = {
      def tag(implicit T:TypeTag[O]) = implicitly[TypeTag[Var[Change[O]]]]

      def newVar = Var(r.last, st getOrElse strategy)

      def getVar = key.fold(newVar)(p ⇒ cache(newVar, p._1)(tag(p._2)))

      def connect(v: Var[Change[O]]) = IO {
        r.node connectChild Node.child{ t ⇒ v.set(r.last); false }
        v.addListener(out)
      }

      getVar flatMap connect
    }

  //@TODO
  private[dire] def trans[A,B](f: A ⇒ IO[B])(in: RawSignal[Event[A]])
    : IO[RawSignal[Event[B]]] = ??? //for {
//      v  ← Varhhh
//
//      si = DataSink.create[Event[A]](
//        _.fold(f(_) flatMap { b ⇒ IO(v.fire(Once(b))) }, IO.ioUnit)
//      )
//
//      so = Var.VarSource[Event[B]]
//      _  ← sink(si)(in)
//      s  ← source[Event[B]](so ini v)(so cb  v)
//      _  ← IO(reactives += v)
//    } yield ???

  private[dire] def timeSignal: IO[RawSignal[Time]] =
    cached[Event[Nothing],Time](
      (_,_) ⇒ source[Time](IO(T0))(Clock(T0, step, _)), "DireTime")(
      Const(Never))

  //lookup a cached RawSignal for the given input and output type. 
  private[dire] def cached[In:TypeTag,Out:TypeTag]
    (sig: (RawSignal[In],Reactor) ⇒ IO[RawSignal[Out]], key: Any)
    (in: RawSignal[In]): IO[RawSignal[Out]] =
    cache(sig(in,this) map { (in,_) }, key) map { _._2 }

  //loops the output of a signal function asynchronuously back to
  //its input
  private[dire] def loop[A]
    (ini: ⇒ A)
    (f: (RawSignal[A], Reactor) ⇒ IO[RawSignal[A]]): IO[RawSignal[A]] = for {
    s   ← IO(new RSource[A](ini, this, _ ⇒ IO(IO.ioUnit)))
    _   ← IO(reactives += s)
    r   ← IO(new RawSource(s))
    re  ← f(r, this)
    _   ← IO(re.node connectChild Node.child {t ⇒ s.fire(re.last.v); false })
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
