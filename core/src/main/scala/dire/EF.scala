package dire

import dire.control.{RawEvents, Reactor}
import scalaz._, Scalaz._, effect.IO

case class EF[-A,+B](run: Events[A] ⇒ Events[B]) 
  extends EFHelper[A,B,Events,EF] {
  override protected def fromF[C,D](r: Events[C] ⇒ Events[D]) = EF(r)
  override protected def toF[C,D](g: EF[C,D]) = g.run

  def andThen[C](that: EF[B,C]): EF[A,C] = EF(run andThen that.run)

  def compose[C](that: EF[C,A]): EF[C,B] = that andThen this

  def >=> [C](that: EF[B,C]): EF[A,C] = andThen(that)

  def <=<[C](that: EF[C,A]): EF[C,B] = compose(that)

}

object EF extends EFInstances with EFFunctions

trait EFInstances {
  implicit def EFFunctor[R]: Functor[({type λ[α]=EF[R,α]})#λ] = 
    new Functor[({type λ[α]=EF[R,α]})#λ] {
      def map[A,B](ef: EF[R,A])(f: A ⇒ B) = ef map f
    }
}

trait EFFunctions {
  lazy val processors = Runtime.getRuntime.availableProcessors

  private def neverE[A]: Events[A] = _ ⇒ IO(RawEvents.Never)

  def never[A,B]: EF[A,B] = EF(_ ⇒ neverE)

  def src[A](callback: Out[A] ⇒ IO[IO[Unit]]): EIn[A] =
    EF(_ ⇒ RawEvents src callback)

  def ticks: EIn[Unit] = EF(_ ⇒ RawEvents.ticks)

  def times: EIn[Time] = ticks mapEvent { case Event(t,_) ⇒ t }

  def runReactive[A](in: EIn[A],
                     delay: Time = 1000L,
                     proc: Int = processors): IO[IO[Unit]] = {
      lazy val ex = java.util.concurrent.Executors.newFixedThreadPool(proc)
      lazy val s = scalaz.concurrent.Strategy.Executor(ex)

      for {
        r ← IO(new Reactor(delay)(s))
        _ ← in run neverE apply r
        _ ← r.start
      } yield r.stop >> IO(ex.shutdown())
    }
}

trait EFHelper[-A,+B,F[+_],G[-_,+_]] {
  def run: F[A] ⇒ Events[B]

  protected def fromF[C,D](r: F[C] ⇒ Events[D]): G[C,D]

  protected def toF[C,D](g: G[C,D]): F[C] ⇒ Events[D]

  def filter(p: B ⇒ Boolean): G[A,B] = fromF { fa ⇒ r ⇒ 
    run(fa)(r) flatMap { _ filterIO p }
  }

  def eventTo(o: Out[Event[B]]): G[A,B] = fromF { fa ⇒ r ⇒ 
    run(fa)(r) flatMap { rawA ⇒ rawA onEvent o as rawA }
  }

  def map[C](f: B ⇒ C): G[A,C] = mapEvent { case Event(_,b) ⇒ f(b) }

  def mapEvent[C](f: Event[B] ⇒ C): G[A,C] =
    fromF[A,C] { fa ⇒ r ⇒ run(fa)(r) flatMap { _ mapEventIO f } }

  def merge[C<:A,D>:B](that: G[C,D]): G[C,D] = fromF { fc ⇒ r ⇒ 
    for {
      rawB ← run(fc)(r)
      rawD ← toF(that)(fc)(r)
      res  ← rawB mergeIO rawD
    } yield res
  }

  def to(o: Out[B]): G[A,B] = eventTo(e ⇒ o(e.v))

  def -->(o: Out[B]): G[A,B] = to(o)
}

// vim: set ts=2 sw=2 et:
