package dire.control

import dire.{Event, Out, Time}
import scalaz._, Scalaz._, effect.IO

sealed trait Events[+A] {
  def node: Node

  def fired: IO[List[Event[A]]]

  def on(out: Out[A]): IO[Unit] = onEvent(e ⇒ out(e.v))

  def onEvent(out: Out[Event[A]]): IO[Unit] = for {
    n ← Node.childNode(_ ⇒ fired flatMap { _ foldMap out } as false,
                       IO.ioUnit)
    _ ← node connectChild n
  } yield ()

  def mapIO[B](f: A ⇒ B): IO[Events[B]] = toE(_.map { _ map f }.η[IO])

  def mergeIO[B>:A](that: Events[B]): IO[Events[B]] =
    toE[B](as ⇒ that.fired map { as ::: _ } )

  private def toE[B](f: List[Event[A]] ⇒ IO[List[Event[B]]])
    : IO[Events[B]] = for {
    r ← IO.newIORef[List[Event[B]]](Nil)
    n ← Node.childNode(_ ⇒ fired >>= f >>= { bs ⇒ r write bs as bs.nonEmpty },
                       r write Nil)
    _ ← node connectChild n
  } yield new Events[B]{ val node = n; def fired = r.read }
}

object Events {
  case object Never extends Events[Nothing] {
    val node = Isolated
    def fired = IO(Nil)
  }

  def src[A](callback: Out[A] ⇒ IO[IO[Unit]])(r: Reactor)
    : IO[Events[A]] = for {
      s ← Source.newSource(callback)
      _ ← r addSource s
    } yield new Events[A]{ def node = s.node; def fired = s.fired }
}

// vim: set ts=2 sw=2 et:
