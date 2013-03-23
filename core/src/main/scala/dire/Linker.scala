package dire

import dire.control.Sink
import scalaz._, Scalaz._, effect.IO

/** Highly experimental!
  *
  * Used as a means to identify event sources. A Button in a user
  * interface for instance, should only have one event source in
  * a reactive network. Lets say Buttons fire events of type Unit.
  * We therefore provide an implicit linker from Button to Unit
  * when creating a signal functions that represents button
  * clicks. When setting up the reactive graph, several parts of
  * a complex signal function may try to get hold of an event
  * source for a given button. If such an event source already
  * exists, no new one will be created.
  *
  * Note that values of typeparam K will be used as keys in an
  * immutable Map and should therefore be usable as such.
  *
  * Note also, that either the key or the value type should be
  * quite specific. A linker of type Linker[String,Unit] can mean
  * a whole bunch of things.
  * 
  */
sealed trait EventSource[Key,Src,Value] {
  private[dire] def ini(s: Src): IO[Value]
  private[dire] def cb(s: Src): Out[Value] ⇒ IO[IO[Unit]]
}

object EventSource extends EventSourceFunctions {
  def apply[K,S,V](implicit S: EventSource[K,S,V]): EventSource[K,S,V] = S
}

trait EventSourceFunctions {
  final def signalSrc[K,S,V](initial: S ⇒ IO[V])
                      (callback: S ⇒ Out[V] ⇒ IO[IO[Unit]])
                      : EventSource[K,S,V] =
    new EventSource[K,S,V] {
      def ini(s: S) = initial(s)
      def cb(s: S) = callback(s)
    }

  def signalSrcInpure[K,S,V](initial: S ⇒ V)
                            (callback: S ⇒ Sink[V] ⇒ Sink[Unit])
                            : EventSource[K,S,V] =
    signalSrc[K,S,V](s ⇒ IO(initial(s)))(s ⇒ o ⇒ IO {
      val sink = callback(s)(v ⇒ o(v).unsafePerformIO)

      IO { sink apply () }
    })

  def eventSrc[K,S,V](callback: S ⇒ Out[V] ⇒ IO[IO[Unit]])
    : EventSource[K,S,Event[V]] =
    signalSrc[K,S,Event[V]](_ ⇒ IO(Never))(
      s ⇒ oe ⇒ callback(s)( v ⇒ oe(Once(v))))

  def eventSrcInpure[K,S,V](callback: S ⇒ Sink[V] ⇒ Sink[Unit])
    : EventSource[K,S,Event[V]] =
    signalSrcInpure[K,S,Event[V]](_ ⇒ Never)(
      s ⇒ se ⇒ callback(s)(v ⇒ se(Once(v))))
}

trait EventSourceInstances {
  import dire.control.Clock
  import EventSource.eventSrcInpure

  private[dire] implicit val ticks: EventSource[Symbol,Time,Event[Unit]] = 
    eventSrcInpure[Symbol,Time,Unit](t ⇒ su ⇒ Clock(T0, t, _ ⇒ su(())))
}

// vim: set ts=2 sw=2 et:
