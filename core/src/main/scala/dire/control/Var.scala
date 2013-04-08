package dire.control

import dire.{Out, DataSource, DataSink}
import java.util.concurrent.{CountDownLatch ⇒ CDL}
import scalaz._, Scalaz._, effect.IO
import scalaz.concurrent.{Strategy, Actor}

sealed abstract class Var[A](ini: A, s: Strategy) {
  import Var._

  private[this] val listeners =
    new collection.mutable.ListBuffer[Out[A]]

  private[this] var active = true

  private[this] var actual = ini

  private[this] val actor: Actor[VarEvent[A]] =
    new Actor[VarEvent[A]](react)(s)

  private[this] def react(e: VarEvent[A]) = e match {
    case Add(o, c)    ⇒ if(active) {
      listeners += o
      o(actual).unsafePerformIO()
      c.countDown()
    } else c.countDown()

    case Remove(o, c) ⇒ if(active) {
      listeners -= o
      c.countDown()
    } else (c.countDown())

    case Get(o, c) ⇒ if(active) {
      o(actual).unsafePerformIO()
      c.countDown()
    } else (c.countDown())

    case Fire(a)      ⇒ if (active) {
      listeners foreach { _(a).unsafePerformIO() }
    }

    case Shutdown(c) ⇒ if (active) {
      active = false
      actor ! Dead(c)
    }

    case Dead(c) ⇒ c.countDown()
  }

  private[control] def addListener(o: Out[A]) {
    await(1, c ⇒ actor ! Add(o, c))
  }

  private[control] def removeListener(o: Out[A]) {
    await(1, c ⇒ actor ! Remove(o, c))
  }

  private[control] def fire(a: A) { actor ! Fire(a) }

  private[control] def get: A = {
    var res: A = actual
    await(1, c ⇒ actor ! Get(a ⇒ IO(res = a), c))
    res
  }
  
  private[control] def shutdown {
    await(1, c ⇒ actor ! Shutdown(c))
  }
}

object Var {
  private[control] def apply[A](a: A, s: Strategy): IO[Var[A]] = 
    IO { new Var(a, s){} }

  implicit def VarSink[A]: DataSink[Var[A],A] = 
    DataSink.create[Var[A],A](v ⇒ a ⇒ IO(v.fire(a)), _ ⇒ IO.ioUnit)

  implicit def VarSource[A]: DataSource[Var[A],A] = 
    DataSource.signalSrc[Var[A],A](s ⇒ IO(s.get))(
      s ⇒ o ⇒ IO(s.addListener(o)) as IO(s.removeListener(o))) 

  private sealed trait VarEvent[A]

  private case class Get[A](o: Out[A], cdl: CDL) extends VarEvent[A]
  private case class Add[A](o: Out[A], cdl: CDL) extends VarEvent[A]
  private case class Remove[A](o: Out[A], cdl: CDL) extends VarEvent[A]
  private case class Fire[A](a: A) extends VarEvent[A]
  private case class Shutdown[A](cdl: CDL) extends VarEvent[A]
  private case class Dead[A](cdl: CDL) extends VarEvent[A]
}

// vim: set ts=2 sw=2 et:
