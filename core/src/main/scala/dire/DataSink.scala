package dire

import dire.control.{Reactor, RawSignal ⇒ RS}
import scala.reflect.runtime.universe.TypeTag
import scalaz._, Scalaz._, effect.IO
import scalaz.concurrent.Strategy

/** Consumes values using a declared concurrency strategy */
sealed trait DataSink[-A] { self ⇒ 
  private[dire] def connect(raw: RS[A], r: Reactor): IO[Unit]

  def contramap[B](f: B ⇒ A): DataSink[B] = new DataSink[B] {
    private[dire] def connect(raw: RS[B], r: Reactor) =
      raw map f flatMap { self.connect(_, r) }
  }
}

object DataSink extends DataSinkFunctions with DataSinkInstances

trait DataSinkFunctions {

  final def create[A](out: Out[A], 
                      clean: IO[Unit] = IO.ioUnit,
                      strategy: Option[Strategy] = None): DataSink[A] =
    createE[A](eOut(out), clean, strategy)

  final def sync[A](out: Out[A], 
                    clean: IO[Unit] = IO.ioUnit): DataSink[A] =
    createE[A](eOut(out), clean, Some(Strategy.Sequential))

  final def async[A](out: Out[A], 
                     clean: IO[Unit] = IO.ioUnit): DataSink[A] =
    createE[A](eOut(out), clean)

  final def cached[A](out: Out[A], 
                      key: Any,
                      clean: IO[Unit] = IO.ioUnit,
                      strategy: Option[Strategy] = None)
                      (implicit T: TypeTag[A]): DataSink[A] = 
    createE[A](eOut(out), clean, strategy, Some((key, T)))

  final private[dire] def createE[A](
    out: Out[Event[A]], 
    clean: IO[Unit] = IO.ioUnit,
    strategy: Option[Strategy] = None,
    key: Option[(Any,TypeTag[A])] = None): DataSink[A] = new DataSink[A] {
      private[dire] def connect(raw: RS[A], r: Reactor): IO[Unit] =
        r.sink[A](out, clean, strategy, key)(raw)
    }

  final private[dire] def syncE[A](o: Out[Event[A]]): DataSink[A] =
    createE(o, strategy = Some(Strategy.Sequential))

  private def eOut[A](o: Out[A]): Out[Event[A]] = _.fold(o, IO.ioUnit)

  private final def empty[A]: DataSink[A] = sync[A](_ ⇒ IO.ioUnit)
}

trait DataSinkInstances {
  implicit val DataSinkContravariant: Contravariant[DataSink] =
    new Contravariant[DataSink] {
      def contramap[A,B](r: DataSink[A])(f: B ⇒ A) = r contramap f
    }

  implicit def DataSinkMonoid[A]: Monoid[DataSink[A]] =
    new Monoid[DataSink[A]] {
      val zero = DataSink.sync[A](_ ⇒ IO.ioUnit)
      def append(a: DataSink[A], b: ⇒ DataSink[A]) = new DataSink[A] {
        private[dire] def connect(raw: RS[A], r: Reactor): IO[Unit] =
          a.connect(raw, r) >> b.connect(raw, r)
      }
    }
}

// vim: set ts=2 sw=2 et:
