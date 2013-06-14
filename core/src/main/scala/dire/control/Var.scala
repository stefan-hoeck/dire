package dire.control

import dire.{Out, DataSource, DataSink, SIn, SF, StrategyO}
import java.util.concurrent.{CountDownLatch ⇒ CDL}
import scala.reflect.runtime.universe.TypeTag
import scalaz._, Scalaz._, effect.IO
import scalaz.concurrent.{Strategy, Actor}

final class Var[A] private (ini: A, s: Strategy)
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

    case Mod(f)      ⇒ if (active) {
      actual = f(actual)
      if(started) notify()
    }
  }

  /** Sets a new value for this `Var`
    *
    * It is recommended to typically modify the contents
    * of a `Var` in a reactive setup either by using
    * `sf` or `sink`. This IO-action should only be used
    * on those rare occasions where a `Var` needs to be
    * modified outside of a reactive setup.
    */
  def put(a: A): IO[Unit] = IO(set(a))

  /** Modifies the actual value of this `Var`
    *
    * It is recommended to typically modify the contents
    * of a `Var` in a reactive setup either by using
    * `sf` or `sink`. This IO-action should only be used
    * on those rare occasions where a `Var` needs to be
    * modified outside of a reactive setup.
    */
  def mod(f: A ⇒ A): IO[Unit] = IO(modify(f))

  /** Gets the actual value of this `Var`
    *
    * It is recommended to typically access the contents
    * of a `Var` in a reactive setup either by using
    * `sf` or `in`. This IO-action should only be used
    * on those rare occasions where a `Var` needs to be
    * accessed outside of a reactive setup.
    */
  def read: IO[A] = IO(get)

  protected def doStart() { started = true; notify() }

  protected def doStop() { listeners.clear() }

  private[control] def addListener(o: Out[A]) {
    await(1, c ⇒ fire(Add(o, c)))
  }

  private[control] def removeListener(o: Out[A]) {
    await(1, c ⇒ fire(Remove(o, c)))
  }

  private[control] def set(a: A) { fire(Mod(_ ⇒ a)) }

  private[control] def modify(f: A ⇒ A) { fire(Mod(f)) }

  private[control] def get: A = {
    var res: A = actual
    await(1, c ⇒ fire(Get(a ⇒ IO(res = a), c)))
    res
  }
  
  private[dire] def shutdown() { await(1, stop) }

  import Var.VarSource

  /** This Var's input signal function
    *
    * This can be used to use this mutable `Var` in a reactive
    * setting and is the preferred way of reacting on changes
    * made to this `Var`.
    */
  def in: SIn[A] = SF src this

  /** Same as `in` but cached (see `SF.cached`) */
  def cachedIn(implicit T: TypeTag[A]): SIn[A] = SF cachedSrc this

  /** This `Var`'s data sink
    *
    * Use this sink as part of a reactive setup to modify
    * the contents of this `Var`
    */
  def sink: DataSink[A] = DataSink async put

  /** Returns a signal function that can be used to both modify
    * and react on the contents of this `Var` in a reactive
    * setup.
    */
  def sf: SF[A,A] = (SF.id[A] to sink) >> in

  /** Same as `sf` but cached (see `SF.cached`) */
  def cachedSf(implicit T: TypeTag[A]): SF[A,A] = SF cached (sf, this)
}

object Var {
  private[control] def apply[A](a: A, s: Strategy): IO[Var[A]] = 
    IO { new Var(a, s) }

  /** Creates a new mutable value */
  def newVar[A](a: A): IO[Var[A]] = for {
    v ← apply(a, Strategy.Sequential)
    _ ← IO(v.start())
  } yield v

  private[control] def forReactor[A](
    a: A, r: Reactor, s: StrategyO): IO[Var[A]] = 
    apply(a, s getOrElse r.strategy) >>= { v ⇒ r.addReactive(v) as v }

  implicit def VarSource[A]: DataSource[Var[A],A] = 
    DataSource.eventSrc[Var[A],A](
      s ⇒ o ⇒ IO(s.addListener(o)) as IO(s.removeListener(o))) 

  private[control] sealed trait VarEvent[A]

  private case class Get[A](o: Out[A], cdl: CDL) extends VarEvent[A]
  private case class Add[A](o: Out[A], cdl: CDL) extends VarEvent[A]
  private case class Remove[A](o: Out[A], cdl: CDL) extends VarEvent[A]
  private case class Mod[A](f: A ⇒ A) extends VarEvent[A]
}

// vim: set ts=2 sw=2 et:
