package dire

import dire.control.Sink
import scalaz._, Scalaz._, effect.IO

/** Type class that describes how an object of type
  * `Src` can be a data source for values of type `Value`
  *
  * @tparam Src  type of the data source objects
  * @tparam Value type of the events fired by objects of type `Src`
  */
sealed trait DataSource[Src,Value] {
  private[dire] def ini(s: Src): IO[Event[Value]]
  private[dire] def cb(s: Src): Out[Value] ⇒ IO[IO[Unit]]
}

object DataSource extends DataSourceFunctions with DataSourceInstances {
  def apply[S,V](implicit S: DataSource[S,V]): DataSource[S,V] = S
}

trait DataSourceFunctions {

  /** Defines a data source from which signals or event
    * streams can be created
    *
    * @tparam S  type of the data source
    * @tparam V  type of the events fired by the data source
    *
    * @param initial  create the initial value. If this is `None`
    *                 the result will be an event stream that does
    *                 not hold an initial value, otherwise the result
    *                 is a signal.
    * @param callback registers a callback for events of type `V`
    *                 at the source and returns an IO-action, which,
    *                 when invoked, will remove the registered
    *                 callback to cleanup resources
    */
  def create[S,V]
    (initial: S ⇒ IO[Option[V]])
    (callback: S ⇒ Out[V] ⇒ IO[IO[Unit]]): DataSource[S,V] =
      new DataSource[S,V] {
        def ini(s: S) = initial(s) map Event.apply
        def cb(s: S) = callback(s)
      }

  /** Same as `create` but for interacting with inpure
    * third-party libraries
    */
  final def createInpure[S,V]
    (initial: S ⇒ Option[V])
    (callback: S ⇒ Sink[V] ⇒ Sink[Unit]): DataSource[S,V] =
    create[S,V](s ⇒ IO(initial(s)))(impureCb(callback))

  /** Defines a data source from which signals can be created
    *
    * @tparam S  type of the data source
    * @tparam V  type of the events fired by the data source
    *
    * @param initial  create the initial value of the signal
    * @param callback registers a callback for events of type `V`
    *                 at the source and returns an IO-action, which,
    *                 when invoked, will remove the registered
    *                 callback to cleanup resources
    */
  final def signalSrc[S,V]
    (initial: S ⇒ IO[V])
    (callback: S ⇒ Out[V] ⇒ IO[IO[Unit]]): DataSource[S,V] =
    create[S,V](initial(_) map Some.apply)(callback)

  /** Same as `signalSrc` but for interacting with inpure
    * third-party libraries
    */
  final def signalSrcInpure[S,V]
    (initial: S ⇒ V)
    (callback: S ⇒ Sink[V] ⇒ Sink[Unit]): DataSource[S,V] =
    signalSrc[S,V](s ⇒ IO(initial(s)))(impureCb(callback))

  /** Defines a data source from which event streams can be created
    *
    * @tparam S  type of the data source
    * @tparam V  type of the events fired by the data source
    *
    * @param callback registers a callback for events of type `V`
    *                 at the source and returns an IO-action, which,
    *                 when invoked, will remove the registered
    *                 callback to cleanup resources
    */
  final def eventSrc[S,V]
    (callback: S ⇒ Out[V] ⇒ IO[IO[Unit]]): DataSource[S,V] =
    DataSource.create[S,V](_ ⇒ IO(None))(callback)

  /** Same as `eventSrc` but for interacting with inpure
    * third-party libraries
    */
  final def eventSrcInpure[S,V]
    (callback: S ⇒ Sink[V] ⇒ Sink[Unit]): DataSource[S,V] =
    eventSrc[S,V](impureCb(callback))

  private def impureCb[S,V]
    (cb: S ⇒ Sink[V] ⇒ Sink[Unit])
    : S ⇒ Out[V] ⇒ IO[IO[Unit]] = s ⇒ o ⇒ IO {
      val sink = cb(s)(v ⇒ o(v).unsafePerformIO())

      IO { sink apply () }
    }
}

trait DataSourceInstances {
  import dire.control.Clock
  import DataSource.{eventSrcInpure, eventSrc}

  private[dire] implicit val ticks: DataSource[Time,Unit] = 
    eventSrc[Time,Unit](t ⇒ o ⇒ Clock(T0, t, _ ⇒ o apply ()))

  private[dire] def all[A](as: ⇒ List[A]): DataSource[Unit,A] =
    eventSrcInpure[Unit,A](_ ⇒ su ⇒ {as foreach su; identity})
}

// vim: set ts=2 sw=2 et:
