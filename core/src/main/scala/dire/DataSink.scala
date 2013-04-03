package dire

import dire.control.Sink
import scalaz._, Scalaz._, effect.IO
import scalaz.concurrent.Strategy

/** Describes how an object of type `Snk` can operate as a data sink
  * that consumes values of type `Value`.
  */
sealed trait DataSink[-Snk,-Value] {
  private[dire] def out(s: Snk): Out[Change[Value]]
  private[dire] def cleanSink(s: Snk): IO[Unit]
  private[dire] def strategy: Option[Strategy]
}

object DataSink extends DataSinkFunctions

trait DataSinkFunctions {
  private[dire] def createC[S,A](
    o: S ⇒ Out[Change[A]], 
    cl: S ⇒ IO[Unit] = (s: S) ⇒ IO.ioUnit,
    st: Option[Strategy] = None): DataSink[S,A] =
  new DataSink[S,A] {
    private[dire] def out(s: S) = o(s)
    private[dire] def cleanSink(s: S) = cl(s)
    private[dire] def strategy = st
  }

  private[dire] def synchC[A](o: Out[Change[A]]): DataSink[Unit,A] =
    createC(_ ⇒ o, st = Some(Strategy.Sequential))

  def create[S,A](o: S ⇒ Out[A], 
                  cl: S ⇒ IO[Unit],
                  st: Option[Strategy] = None): DataSink[S,A] =
    createC[S,A](s ⇒ ca ⇒ o(s)(ca.v), cl, st)

  def createE[S,A](o: S ⇒ Out[A], 
                   cl: S ⇒ IO[Unit],
                   st: Option[Strategy] = None): DataSink[S,Event[A]] =
    create[S,Event[A]](s ⇒ ea ⇒ ea.fold(o(s), IO.ioUnit), cl, st)
}

// vim: set ts=2 sw=2 et:
