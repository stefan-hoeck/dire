package dire

import dire.control.Sink
import scalaz._, Scalaz._, effect.IO
import scalaz.concurrent.Strategy

/** Consumes values using a declared concurrency strategy */
sealed trait DataSink[-A] {
  private[dire] def out: Out[Change[A]]
  private[dire] def cleanSink: IO[Unit]
  private[dire] def strategy: Option[Strategy]

  private[dire] def lift[F[+_]](implicit I: IdOrEvent[F])
    : DataSink[F[A]] = DataSink.createC[F[A]](cfv ⇒
      I toEvent cfv.v fold (v ⇒ out(cfv as v), IO.ioUnit),
      cleanSink,
      strategy)
}

object DataSink extends DataSinkFunctions

trait DataSinkFunctions {
  private[dire] def createC[A](
    o: Out[Change[A]], 
    cl: IO[Unit] = IO.ioUnit,
    st: Option[Strategy] = None): DataSink[A] =
  new DataSink[A] {
    private[dire] val out = o
    private[dire] val cleanSink = cl
    private[dire] val strategy = st
  }

  private[dire] def syncC[A](o: Out[Change[A]]): DataSink[A] =
    createC(o, st = Some(Strategy.Sequential))

  def create[A](out: Out[A], 
                clean: IO[Unit] = IO.ioUnit,
                strategy: Option[Strategy] = None): DataSink[A] =
    createC[A](ca ⇒ out(ca.v), clean, strategy)

  def sync[A](out: Out[A], 
              clean: IO[Unit] = IO.ioUnit): DataSink[A] =
    createC[A](ca ⇒ out(ca.v), clean, Some(Strategy.Sequential))

  def async[A](out: Out[A], 
               clean: IO[Unit] = IO.ioUnit): DataSink[A] =
    createC[A](ca ⇒ out(ca.v), clean)
}

// vim: set ts=2 sw=2 et:
