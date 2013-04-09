package dire

import dire.control.{RawSignal ⇒ RS, Reactor}
import scalaz._, Scalaz._, effect.IO
import scalaz.Leibniz.===

/** Represents a signal transformation.
  *
  * This class offers a rich set of combinators needed to describe
  * reactive networks.
  */
//@ TODO: Implement `take` and `drop` for event streams
class SfT[-A,+B,I[+_],O[+_]] private[dire](
    private[dire] val run: (RS[I[A]], Reactor) ⇒ IO[RS[O[B]]])
    (implicit private val MI: Monad[I], private val MO: Monad[O]) {
  import SfT.{sync1, sync2, cApp}, DataSink._

  /** Applies a time-changing function to the signals values */
  def ap[C,AA<:A](f: SfT[AA,B ⇒ C,I,O]): SfT[AA,C,I,O] =
    (f <*> this)(_ apply _)

  /** Sequentially combines two signal functions */
  def andThen[C,O1[+_]](that: SfT[B,C,O,O1]): SfT[A,C,I,O1] =
    that compose this

  /** Like `syncTo` but performs side effects asynchronuously,
    * that is, the main `Reactor` thread is not blocked.
    *
    * The reactive system guarantees that, as long as the given
    * `out` is not used as a data sink for other signals, it will
    * only be accessed sequentially from this signal, i.e. it
    * need not be thread safe. It is for instance possible to
    * write all values of this signal to a file without being
    * afraid of race conditions or other ugliness. If the same
    * file is used to monitor several distinct signals or
    * event streams they should either be merged in a single
    * signal (event stream) first, or function `to` should
    * be used which guarantees that all access from a given
    * reactive graph will be sequential.
    */
  def asyncTo(out: Out[O[B]]): SfT[A,B,I,O] =
    toSink(())(DataSink.create[Unit,O[B]](_ ⇒  out, _ ⇒ IO.ioUnit))

  /** Creates an event stream that fires whenever this signal's
    * value changes to a new one that is distinct from the old
    * value.
    *
    * The resulting event stream starts with this signal's initial value
    */
  def changes[BB>:B](implicit E: Equal[O[BB]], S: O[BB] === Id[BB])
    : SfT[A,BB,I,Event] = distinct[BB].ef

  private[dire] def changeTo(out: Out[Change[O[B]]]): SfT[A,B,I,O] =
    toSink(())(DataSink synchC out)

  /** Sequentially combines two signal functions */
  def compose[C,O1[+_]](that: SfT[C,A,O1,I]): SfT[C,B,O1,O] =
    SfT[C,B,O1,O]((rc, r) ⇒ that.run(rc,r) >>= { run(_, r) })(that.MI,MO)

  /** Contravariant mapping */
  def contramap[C](f: C ⇒ A): SfT[C,B,I,O] = compose(SfT.id[C,I] map f)

  /** Returns a signal that only fires an event if its new
    * value is different from its old one
    */
  def distinct[BB>:B](implicit E: Equal[O[BB]]): SfT[A,BB,I,O] =
    sync1(this)(identity)((cb,cc) ⇒ if ((cb.v: O[BB]) ≟ cc.v) cc else cb)

  /** Creates an event stream that fires whenever this signal
    * fires an event.
    *
    * This is similar to `changes` but fires even if the signal's
    * new value is not distinct from the old one. Note that unlike
    * `events` the first event will be fired at `T0` with the
    * signal's initial value.
    */
  def ef[BB>:B](implicit L: O[BB] === Id[BB]): SfT[A,BB,I,Event] = {
    def f(c: Change[BB]): Change[Event[BB]] = c map Once.apply

    sync1(substO[BB,Id])(f)((cb,_) ⇒ f(cb))
  }

  /** Creates an event stream that fires whenever this signal
    * fires an event.
    *
    * Note that unlike with function `ef`, the resulting event stream
    * will skip the signal's initial value.
    */
  //@TODO implement
  def events[BB>:B](implicit L: O[BB] === Id[BB]): SfT[A,BB,I,Event] = ??? //ef drop 1

  /** Functor map */
  def map[C](f: B ⇒ C): SfT[A,C,I,O] =
    sync1(this)(cApp[O].map(_)(f))((b,_) ⇒ cApp[O].map(b)(f))

  private def substO[BB>:B,O1[+_]]
    (implicit L: O[BB] === O1[BB]): SfT[A,BB,I,O1] = this.asInstanceOf

  /** Returns an event stream that fires this signals actual value
    * whenever the given event stream fires
    */
  def on[C,AA<:A,BB>:B](ef: SfT[AA,C,I,Event])(implicit L: O[BB] === Id[BB])
    : SfT[AA,BB,I,Event] = upon[C,BB,AA,BB](ef)((bb,_) ⇒ bb)

  /** Performs the given IO-action with this signal's initial value
    * and whenever this signal changes.
    *
    * Note that `out` will be called by the `Reactor`'s
    * running thread that updates
    * the main reactive graph. Therefore, side effects performed
    * by `out` need to be fast if the system should stay reactive.
    * Consider using `asyncTo` instead for fully asynchronuous
    * side effects or when side effects have to be performed in a
    * special type of thread (the Swing event dispatch thread for instance).
    */
  def syncTo(out: Out[O[B]]): SfT[A,B,I,O] = changeTo(c ⇒ out(c.v))

  /** Connect a reactive branch to this signal function but
    * return to the original branch afterwards.
    */
  def to[C,O1[+_]](that: SfT[B,C,O,O1]): SfT[A,B,I,O] = 
    SfT { (ra,r) ⇒ run(ra, r) >>= { rb ⇒ that.run(rb,r) as rb } }

  /** Connect a reactive branch that consumes events
    * to this signal function but
    * return to the original branch afterwards.
    */
  def toE[C,BB>:B,O1[+_]](that: SfT[BB,C,Event,O1])
                         (implicit L: O[BB] === Id[BB]): SfT[A,BB,I,Id] = 
    substO[BB,Id] to (SfT.idS[BB].ef to that)

  /** Asynchronuously output the values of this signal to a data sink
    *
    * How the data sink operates and what concurrency strategy is
    * applied is defined in the [[dire.DataSink]] type class.
    */
  def toSink[S](s: S)(implicit D: DataSink[S,O[B]]): SfT[A,B,I,O] = 
    SfT { (ra,r) ⇒ run(ra,r) >>= { r.sink(s, _) } }

  /** Combines a signal with an event stream through a
    * function of arity two.
    *
    * The resulting event stream fires only, when the given
    * event stream fires.
    */
  def upon[C,D,AA<:A,BB>:B](ef: SfT[AA,C,I,Event])
                           (f: (BB,C) ⇒ D)
                           (implicit L: O[BB] === Id[BB])
                           : SfT[AA,D,I,Event] = {
    def g(cb: Change[BB], cec: Change[Event[C]]): Option[Change[Event[D]]] =
      if (cec.at >= cb.at) Some(cec map { _ map { f(cb.v,_) } })
      else None

    def ini(cb: Change[BB], cec: Change[Event[C]]) = g(cb,cec) | cb.as(Never)

    sync2(substO[BB,Id],ef)(ini)((cb,cec,ced) ⇒ g(cb,cec) | ced)
  }

  /** Alias for `andThen` */
  def >=>[C,O1[+_]](that: SfT[B,C,O,O1]): SfT[A,C,I,O1] = andThen(that)

  /** Alias for `compose` */
  def <=<[C,O1[+_]](that: SfT[C,A,O1,I]): SfT[C,B,O1,O] = compose(that)

  /** Combines two signals with a pure function */
  def <*>[C,D,AA<:A](that: SfT[AA,C,I,O])(f: (B,C) ⇒ D): SfT[AA,D,I,O] =
    sync2(this, that)(cApp[O].lift2(f))((b,c,_) ⇒ cApp[O].lift2(f)(b,c))

  /** Alias for `contramap`*/
  def ∙ [C](f: C ⇒ A): SfT[C,B,I,O] = contramap(f)

  /** Alias for `syncTo` */
  def -->(out: Out[O[B]]): SfT[A,B,I,O] = syncTo(out)

  /** Alias for `to` */
  def >|>[C,O1[+_]](that: SfT[B,C,O,O1]): SfT[A,B,I,O] = to(that)
}

object SfT extends SfTFunctions {

  private[dire] def apply[A,B,I[+_]:Monad,O[+_]:Monad]
    (run: (RS[I[A]], Reactor) ⇒ IO[RS[O[B]]]): SfT[A,B,I,O] =
    new SfT(run)

  private[dire] lazy val processors =
    Runtime.getRuntime.availableProcessors
}

trait SfTFunctions {
  /** Event stream function from a pure function */
  def ef[A,B](f: A ⇒ B): EF[A,B] = idE map f

  /** The identity signal function */
  def idS[A]: SF[A,A] = id

  /** The identity event stream function */
  def idE[A]: EF[A,A] = id

  /** Signal function from a pure function */
  def sf[A,B](f: A ⇒ B): SF[A,B] = idS map f

  private[dire] def id[A,F[+_]: Monad]: SfT[A,A,F,F] = SfT((a,_) ⇒ IO(a))

  /** Creates a derrived signal depending on two input signals
    * that is synchronously updated whenever one of the two
    * input signals changes
    *
    * @param sa the first of the two signal functions to be
    *           combined
    *
    * @param sa the second of the two signal functions to be
    *           combined
    *
    * @param ini  calculates the initial value of the new
    *             signal from the two input signals' initial
    *             values
    *
    * @param next  calculates the new value of the derived
    *              signal from the two actual values of the
    *              input signals and the derived signal's
    *              latest value
    */
  private[dire] def sync2[R,A,B,C,I[+_]:Monad,O1[+_],O2[+_],O3[+_]:Monad]
    (sa: SfT[R,A,I,O1], sb: SfT[R,B,I,O2])
    (ini: Initial2[O1[A],O2[B],O3[C]])
    (next: Next2[O1[A],O2[B],O3[C]]): SfT[R,C,I,O3] =
    SfT[R,C,I,O3]((rr, r) ⇒ 
      for {
        ra ← sa.run(rr, r)
        rb ← sb.run(rr, r)
        rc ← RS.sync2(ra, rb)(ini)(next)
      } yield rc
    )

  /** Creates a derrived signal depending on one input signal
    * that is synchronously updated whenever the
    * input signal changes
    *
    * @param sa   the input signal functions
    *
    * @param ini  calculates the initial value of the new
    *             signal from the input signal's initial
    *             value
    *
    * @param next  calculates the new value of the derived
    *              signal from the actual value of the
    *              input signal and the derived signal's
    *              latest value
    */
  private[dire] def sync1[R,A,B,I[+_]:Monad,O1[+_],O2[+_]:Monad]
    (sa: SfT[R,A,I,O1])
    (ini: Initial1[O1[A],O2[B]])
    (next: Next1[O1[A],O2[B]]): SfT[R,B,I,O2] =
    SfT[R,B,I,O2]((rr, r) ⇒ 
      for {
        ra ← sa.run(rr, r)
        rb ← RS.sync1(ra)(ini)(next)
      } yield rb
    )

  private[dire] def cApp[F[+_]:Applicative] =
    Applicative[Change].compose[F]

}

// vim: set ts=2 sw=2 et:
