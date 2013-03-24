package dire

import dire.control.Sink
import scalaz._, Scalaz._, effect.IO

sealed trait SourceSignal[Src,Value] {
  private[dire] def ini(s: Src): IO[Value]
  private[dire] def cb(s: Src): Out[Value] ⇒ IO[IO[Unit]]
}

object SourceSignal extends SourceSignalFunctions with SourceSignalInstances {
  def apply[S,V](implicit S: SourceSignal[S,V]): SourceSignal[S,V] = S
}

trait SourceSignalFunctions {
  final def signalSrc[S,V](initial: S ⇒ IO[V])
                          (callback: S ⇒ Out[V] ⇒ IO[IO[Unit]])
                          : SourceSignal[S,V] =
    new SourceSignal[S,V] {
      def ini(s: S) = initial(s)
      def cb(s: S) = callback(s)
    }

  def signalSrcInpure[S,V](initial: S ⇒ V)
                          (callback: S ⇒ Sink[V] ⇒ Sink[Unit])
                          : SourceSignal[S,V] =
    signalSrc[S,V](s ⇒ IO(initial(s)))(s ⇒ o ⇒ IO {
      val sink = callback(s)(v ⇒ o(v).unsafePerformIO)

      IO { sink apply () }
    })

  def eventSrc[S,V](callback: S ⇒ Out[V] ⇒ IO[IO[Unit]])
    : SourceSignal[S,Event[V]] =
    signalSrc[S,Event[V]](_ ⇒ IO(Never))(
      s ⇒ oe ⇒ callback(s)( v ⇒ oe(Once(v))))

  def eventSrcInpure[S,V](callback: S ⇒ Sink[V] ⇒ Sink[Unit])
    : SourceSignal[S,Event[V]] =
    signalSrcInpure[S,Event[V]](_ ⇒ Never)(
      s ⇒ se ⇒ callback(s)(v ⇒ se(Once(v))))
}

trait SourceSignalInstances {
  import dire.control.Clock
  import SourceSignal.{eventSrcInpure, signalSrcInpure}

  private[dire] implicit val ticks: SourceSignal[Time,Event[Unit]] = 
    eventSrcInpure[Time,Unit](t ⇒ su ⇒ Clock(T0, t, _ ⇒ su(())))

  private[dire] implicit val time: SourceSignal[Time,Time] = 
    signalSrcInpure[Time,Time](_ ⇒ T0)(t ⇒ su ⇒ Clock(T0, t, su))
}

// vim: set ts=2 sw=2 et:
