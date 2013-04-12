package dire.control

import dire.{Out, DataSource, DataSink}
import java.util.concurrent.{CountDownLatch ⇒ CDL}
import scalaz._, Scalaz._, effect.IO
import scalaz.concurrent.{Strategy, Actor}

sealed abstract class Var[A](ini: A, s: Strategy)
    extends DireActor[Var.VarEvent[A]](s) {
  import Var._

  private[this] val listeners =
    new collection.mutable.ListBuffer[Out[A]]

  private[this] var actual = ini
  
  private[this] var started = false

  private[this] def notify() {
    listeners foreach { _(actual).unsafePerformIO() }
  }
  protected def doRun(v: VarEvent[A], active: Boolean) = v match {
    case Add(o, c)    ⇒ if (active) {
      listeners += o
      if(started) o(actual).unsafePerformIO()
      c.countDown()
    } else c.countDown()

    case Remove(o, c) ⇒ if (active) {
      listeners -= o
      c.countDown()
    } else (c.countDown())

    case Get(o, c)    ⇒ if (active) {
      o(actual).unsafePerformIO()
      c.countDown()
    } else (c.countDown())

    case Fire(a)      ⇒ if (active) { actual = a; if(started) notify() }
  }

  protected def doStart() { started = true; notify() }

  protected def doStop() { listeners.clear() }

  private[control] def addListener(o: Out[A]) {
    await(1, c ⇒ fire(Add(o, c)))
  }

  private[control] def removeListener(o: Out[A]) {
    await(1, c ⇒ fire(Remove(o, c)))
  }

  private[control] def set(a: A) { fire(Fire(a)) }

  private[control] def get: A = {
    var res: A = actual
    await(1, c ⇒ fire(Get(a ⇒ IO(res = a), c)))
    res
  }
  
  private[control] def shutdown {
    await(1, stop)
  }
}

object Var {
  private[control] def apply[A](a: A, s: Strategy): IO[Var[A]] = 
    IO { new Var(a, s){} }

  implicit def VarSource[A]: DataSource[Var[A],A] = 
    DataSource.signalSrc[Var[A],A](s ⇒ IO(s.get))(
      s ⇒ o ⇒ IO(s.addListener(o)) as IO(s.removeListener(o))) 

  private[control] sealed trait VarEvent[A]

  private case class Get[A](o: Out[A], cdl: CDL) extends VarEvent[A]
  private case class Add[A](o: Out[A], cdl: CDL) extends VarEvent[A]
  private case class Remove[A](o: Out[A], cdl: CDL) extends VarEvent[A]
  private case class Fire[A](a: A) extends VarEvent[A]
}

// vim: set ts=2 sw=2 et:
