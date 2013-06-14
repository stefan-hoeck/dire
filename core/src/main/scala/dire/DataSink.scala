package dire

import dire.control.{Reactor, RawSignal ⇒ RS}
import scala.reflect.runtime.universe.TypeTag
import scalaz._, Scalaz._, effect.{IO, IORef}

/** Consumes values using a declared concurrency strategy */
sealed trait DataSink[-A] { self ⇒ 
  private[dire] def connect(raw: RS[A], r: Reactor): IO[Unit]

  def contramap[B](f: B ⇒ A): DataSink[B] = new DataSink[B] {
    private[dire] def connect(raw: RS[B], r: Reactor) =
      raw map f flatMap { self.connect(_, r) }
  }
}

object DataSink extends DataSinkFunctions with DataSinkInstances {

}

trait DataSinkFunctions {
  final def sync[A](out: Out[A], 
                    clean: IO[Unit] = IO.ioUnit): DataSink[A] =
    createE[A](eOut(out), clean, SSS)

  final def async[A](out: Out[A], 
                     clean: IO[Unit] = IO.ioUnit): DataSink[A] =
    createE[A](eOut(out), clean, None)

  final def cachedSync[A](out: Out[A], 
                          key: Any,
                          clean: IO[Unit] = IO.ioUnit)
                          (implicit T: TypeTag[A]): DataSink[A] = 
    createE[A](eOut(out), clean, SSS, Some((key, T)))

  final def cachedAsync[A](out: Out[A], 
                           key: Any,
                           clean: IO[Unit] = IO.ioUnit)
                           (implicit T: TypeTag[A]): DataSink[A] = 
    createE[A](eOut(out), clean, None, Some((key, T)))

  final private[dire] def createE[A](
    out: Out[Event[A]], 
    clean: IO[Unit] = IO.ioUnit,
    strategy: StrategyO = None,
    key: Option[(Any,TypeTag[A])] = None): DataSink[A] = new DataSink[A] {
      private[dire] def connect(raw: RS[A], r: Reactor): IO[Unit] =
        r.sink[A](out, clean, strategy, key)(raw)
    }

  final val stdOut: DataSink[Any] = sync(a ⇒ IO.putStrLn(a.toString))

  final def sdtOutS[A:Show]: DataSink[A] = stdOut ∙ { _.shows }

  final def buffer[A](bf: collection.mutable.ListBuffer[A]): DataSink[A] =
    sync(a ⇒ IO(bf += a))

  final def ioRef[A,F[_]:MonadPlus](ref: IORef[F[A]]): DataSink[A] =
    sync(a ⇒ ref mod { _ <+> a.η[F] } void)

  final private[dire] def syncE[A](o: Out[Event[A]]): DataSink[A] =
    createE(o, strategy = SSS)

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
