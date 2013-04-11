package dire

import dire.control.{Reactor, RawSignal ⇒ RS}
import scala.reflect.runtime.universe.TypeTag
import scalaz._, Scalaz._, effect.IO
import scalaz.concurrent.Strategy

/** Consumes values using a declared concurrency strategy */
sealed trait DataSink[-A] { self ⇒ 
  private[dire] def connect[O[+_]:IdOrEvent]
    (raw: RS[O[A]], r: Reactor): IO[Unit]

  private[dire] def contramap[B](f: B ⇒ A): DataSink[B] =
    new DataSink[B] {
      private[dire] def connect[O[+_]](raw: RS[O[B]], r: Reactor)
                                      (implicit I: IdOrEvent[O]) =
      raw map { I.map(_)(f) } flatMap { self.connect(_, r) }
    }
}

object DataSink extends DataSinkFunctions with DataSinkInstances

trait DataSinkFunctions {

  def create[A](out: Out[A], 
                clean: IO[Unit] = IO.ioUnit,
                strategy: Option[Strategy] = None): DataSink[A] =
    createC[A](cOut(out), clean, strategy)

  def sync[A](out: Out[A], 
              clean: IO[Unit] = IO.ioUnit): DataSink[A] =
    createC[A](cOut(out), clean, Some(Strategy.Sequential))

  def async[A](out: Out[A], 
               clean: IO[Unit] = IO.ioUnit): DataSink[A] =
    createC[A](cOut(out), clean)

  def cached[A](out: Out[A], 
                key: Any,
                clean: IO[Unit] = IO.ioUnit,
                strategy: Option[Strategy] = None)
                (implicit T: TypeTag[A]): DataSink[A] = 
    createC[A](cOut(out), clean, strategy, Some((key, T)))

  private[dire] def createC[A](
    out: Out[Change[A]], 
    clean: IO[Unit] = IO.ioUnit,
    strategy: Option[Strategy] = None,
    key: Option[(Any,TypeTag[A])] = None): DataSink[A] = new DataSink[A] {

    private[dire] def connect[O[+_]]
      (raw: RS[O[A]], r: Reactor)
      (implicit I: IdOrEvent[O]): IO[Unit] = {
        val o: Out[Change[O[A]]] = coa ⇒
          I toEvent coa.v fold (a ⇒ out(Change(coa.at, a)), IO.ioUnit)

        val keyO = key map { p ⇒ (p._1, I.tTag(p._2)) }

        r.sink[O[A]](o, clean, strategy, keyO)(raw)
      }
    }

  private[dire] def syncC[A](o: Out[Change[A]]): DataSink[A] =
    createC(o, strategy = Some(Strategy.Sequential))

  private def cOut[A](o: Out[A]): Out[Change[A]] = ca ⇒ o(ca.v)
}

trait DataSinkInstances {
  implicit val DataSinkContravariant: Contravariant[DataSink] =
    new Contravariant[DataSink] {
      def contramap[A,B](r: DataSink[A])(f: B ⇒ A) = r contramap f
    }
}

// vim: set ts=2 sw=2 et:
