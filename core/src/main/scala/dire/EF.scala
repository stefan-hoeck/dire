package dire

import SF.sync2
import scalaz._, Scalaz._, effect.IO

trait EFFunctions {

  def id[A]: EF[Event[A],A] = SF.id

  /** Asynchronuously loops back the output of the given
    * event stream to its input
    */
  def loop[A](ef: EF[Event[A],A]): EIn[A] = SF.loop(ef)(Never)

  /** The event stream that never fires */
  def never[A]: EF[A,Nothing] = SF const Never

  /** Asynchronuously fires the given event once */
  def once[A](a: ⇒ A): EIn[A] = src(())(DataSource once a)

  def run[A](in: EIn[A],
             proc: Int = SF.processors)
            (stop: A ⇒ Boolean): IO[Unit] =
    SF.run[Event[A]](in, proc)(_ fold (stop, false))

  /** Creates an input event stream from an external data source
    *
    * See the [[dire.DataSource]] type class for more details
    */
  def src[S,V](s: S)(implicit Src: DataSource[S,Event[V]]): EIn[V] =
    SF(_ ⇒ _.source(s))

  /** An asynchronous event source that fires at regular
    * intervals.
    *
    * This is very useful as a basic source of events to simulate
    * all kinds of real time applications.
    * Note that an arbitrary number of completely independant
    * event streams can thus be created. 
    */
  def ticks(step: Time): EIn[Unit] = src[Time,Unit](step)

}

trait EFInstances {
  import SF.EFOps

  implicit def EFFunctor[R]: Functor[({type λ[α]=EF[R,α]})#λ] =
    new Functor[({type λ[α]=EF[R,α]})#λ] {
      def map[A,B](a: EF[R,A])(f: A ⇒ B) = a mapE f
    }

  implicit def EFPlus[R]: PlusEmpty[({type λ[α]=EF[R,α]})#λ] =
    new PlusEmpty[({type λ[α]=EF[R,α]})#λ] {
      def empty[A] = EF.never
      def plus[A](a: EF[R,A], b: ⇒ EF[R,A]) = a merge b
    }

  implicit def EFMonoid[A,B]: Monoid[EF[A,B]] = EFPlus[A].monoid
}

object EF extends EFFunctions with EFInstances

// vim: set ts=2 sw=2 et:
