package dire.control

import dire.{Time, T0}
import scala.language.existentials
import scalaz._, Scalaz._, effect._
import scalaz.concurrent.{Actor, Strategy}

final class Reactor(
    sources: IORef[List[Source[_]]],
    callbacks: IORef[List[Node]],
    state: IORef[Reactor.ReactorState],
    countDown: IORef[Int],
    actualTime: IORef[Time],
    delay: Time)(implicit S: Strategy) {
  import Reactor._

  private[this] var clockKill: IO[Unit] = IO.ioUnit
  private[this] val actor = Actor[ReactorEvent](
    e ⇒ state.read >>= { react(_, e) } unsafePerformIO)

  private[control] def addSource[A](src: Source[A]): IO[Unit] =
    IO(actor ! AddSrc(src))

  private def react(st: ReactorState, ev: ReactorEvent): IO[Unit] =
    (st,ev) match {
      case (Embryo, AddSrc(s))        ⇒ sources.mod { s :: _ }.void
      case (Embryo, Start)            ⇒ start
      case (Waiting, Tick(t))         ⇒ collect(t)
      case (Collecting, NodeReady(o)) ⇒ onReady(o)
      case (_, Shutdown)              ⇒ shutdown
      case _                          ⇒ IO.ioUnit
    }

  private def start: IO[Unit] = for {
    _ ← state write Waiting
    k ← Clock(delay, t ⇒ IO(actor ! Tick(t)))
    _ = clockKill = k
    _ ← sources.read >>= { _ foldMap { _.start } }
  } yield ()

  private def kill: IO[Unit] = IO(actor ! Shutdown)

  private def collect(t: Time): IO[Unit] = for {
    _   ← state write Collecting
    _   ← callbacks write Nil
    ss  ← sources.read
    _   ← countDown write (ss.size + 1)
    _   ← actualTime write t
    _   ← ss foldMap { _ collect { o ⇒ IO(actor ! NodeReady(o)) } }
    _   ← onReady(None)
  } yield ()

  private def onReady(o: Option[Node]): IO[Unit] = for {
    cbs ← callbacks mod { o.toList ::: _ }
    cnt ← countDown mod { _ - 1 }
    _   ← if (cnt ≟ 0) update(cbs) else IO.ioUnit
  } yield ()

  /** Updates the whole dependency graph
    *
    * This includes three phases
    *
    * Phase 1: Mark all changed nodes (and their child nodes etc.)
    *          as 'dirty' meaning each of them has to be updated
    *
    * Phase 2: Recalculate all dirty nodes
    *          Children are only recalculated, if all their parents have
    *          already ben recalculated (that is, they are no longer
    *          marked as dirty) and if at least one parent is
    *          marked as having changed.
    *
    * Phase 3: Cleanup all child nodes to release resources for
    *          garbage collection and to set them back to unchanged.
    *          A child is only cleaned up, if it is marked as having
    *          changed.
    */
  private def update(ns: List[Node]): IO[Unit] = for {
    t ← actualTime.read
    _ ← ns foldMap { _.setDirty }
    _ ← ns foldMap { _ calculate t }
    _ ← ns foldMap { _.cleanup }
    _ ← state write Waiting
  } yield ()

  private def shutdown: IO[Unit] = 
    IO.putStrLn("Shutting down timer") >>
    clockKill >>
    (sources.read >>= { _ foldMap { _.stop } } )
}

object Reactor {
  def run(srcs: List[Source[_]], delay: Time)(implicit S: Strategy)
    : IO[IO[Unit]] = for {
      sources    ← IO newIORef srcs
      callbacks  ← IO newIORef List.empty[Node]
      state      ← IO.newIORef[ReactorState](Embryo)
      countDown  ← IO newIORef 0
      actualTime ← IO newIORef 0L
      reactor    = new Reactor(sources, callbacks, state, countDown, actualTime, delay)
      _          ← reactor.start
      _          ← IO.putStrLn("Reactor started")
    } yield reactor.kill

  private[control] sealed trait ReactorState

  private[control] case object Embryo extends ReactorState
  private[control] case object Collecting extends ReactorState
  private[control] case object Waiting extends ReactorState

  private[control] sealed trait ReactorEvent

  private[control] case object Start extends ReactorEvent
  private[control] case object Shutdown extends ReactorEvent
  private[control] case class Tick(t: Time) extends ReactorEvent
  private[control] case class NodeReady(n: Option[Node]) extends ReactorEvent
  private[control] case class AddSrc(s: Source[_]) extends ReactorEvent
}

// vim: set ts=2 sw=2 et:
