package dire

import dire.control.Sink
import scalaz._, Scalaz._, effect.IO

/** Type class that describes how an object of type
  * `Src` can be a data source for values of type `Value`
  *
  * @tparam Src  type of the data source objects
  * @tparam Value type of the events fired by objects of type `Src`
  */
//sealed trait DataSource[Src,Value] {
//  private[dire] def ini(s: Src): IO[Value]
//  private[dire] def cb(s: Src): Out[Value] ⇒ IO[IO[Unit]]
//}
//
//object DataSource extends DataSourceFunctions with DataSourceInstances {
//  def apply[S,V](implicit S: DataSource[S,V]): DataSource[S,V] = S
//}
//
//trait DataSourceFunctions {
//
//  /** Defines a data source from which signals can be created
//    *
//    * @tparam S  type of the data source
//    * @tparam V  type of the events fired by the data source
//    *
//    * @param initial  create the initial value of the signal
//    * @param callback registers a callback for events of type `V`
//    *                 at the source and returns an IO-action, which,
//    *                 when invoked, will remove the registered
//    *                 callback to cleanup resources
//    */
//  final def signalSrc[S,V](initial: S ⇒ IO[V])
//                          (callback: S ⇒ Out[V] ⇒ IO[IO[Unit]])
//                          : DataSource[S,V] =
//    new DataSource[S,V] {
//      def ini(s: S) = initial(s)
//      def cb(s: S) = callback(s)
//    }
//
//  /** Same as `signalSrc` but for interacting with inpure
//    * third-party libraries
//    */
//  final def signalSrcInpure[S,V](initial: S ⇒ V)
//                                (callback: S ⇒ Sink[V] ⇒ Sink[Unit])
//                                : DataSource[S,V] =
//    signalSrc[S,V](s ⇒ IO(initial(s)))(s ⇒ o ⇒ IO {
//      val sink = callback(s)(v ⇒ o(v).unsafePerformIO)
//
//      IO { sink apply () }
//    })
//
//  /** Defines a data source from which event streams can be created
//    *
//    * @tparam S  type of the data source
//    * @tparam V  type of the events fired by the data source
//    *
//    * @param callback registers a callback for events of type `V`
//    *                 at the source and returns an IO-action, which,
//    *                 when invoked, will remove the registered
//    *                 callback to cleanup resources
//    */
//  final def eventSrc[S,V](callback: S ⇒ Out[V] ⇒ IO[IO[Unit]])
//    : DataSource[S,Event[V]] =
//    signalSrc[S,Event[V]](_ ⇒ IO(Never))(
//      s ⇒ oe ⇒ callback(s)( v ⇒ oe(Once(v))))
//
//  /** Same as `eventSrc` but for interacting with inpure
//    * third-party libraries
//    */
//  final def eventSrcInpure[S,V](callback: S ⇒ Sink[V] ⇒ Sink[Unit])
//    : DataSource[S,Event[V]] =
//    signalSrcInpure[S,Event[V]](_ ⇒ Never)(
//      s ⇒ se ⇒ callback(s)(v ⇒ se(Once(v))))
//}
//
//trait DataSourceInstances {
//  import dire.control.Clock
//  import DataSource.{eventSrcInpure, eventSrc}
//
//  private[dire] implicit val ticks: DataSource[Time,Event[Unit]] = 
//    eventSrc[Time,Unit](t ⇒ o ⇒ Clock(T0, t, _ ⇒ o apply ()))
//
//  private[dire] def once[A](a: ⇒ A): DataSource[Unit,Event[A]] =
//    eventSrcInpure[Unit,A](_ ⇒ su ⇒ {su(a); identity})
//}

// vim: set ts=2 sw=2 et:
