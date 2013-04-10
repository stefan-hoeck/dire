package dire

import dire.control.{RawSignal ⇒ RS, Reactor}
import scalaz._, Scalaz._, effect.IO
import scalaz.Isomorphism.<~>
import scalaz.Leibniz.===

/** Represents a signal transformation.
  *
  * This class offers a rich set of combinators needed to describe
  * reactive networks.
  */
//@ TODO: Implement `take` and `drop` for event streams
class SfT[-A,+B,I[+_],O[+_]] private[dire](
    private[dire] val run: (RS[I[A]], Reactor) ⇒ IO[RS[O[B]]])
    (implicit private val MI: IdOrEvent[I],
              private val MO: IdOrEvent[O]) {
  import SfT._, DataSink._, IdOrEvent._

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
  def changes[BB>:B:Equal](implicit W: AsId[O])
    : SfT[A,BB,I,Event] = sfTo(this).distinct[BB].ef

  private[dire] def changeTo(out: Out[Change[O[B]]]): SfT[A,B,I,O] =
    toSink(())(DataSink synchC out)

  /** Map and filter an event stream in one run */
  def collect[C](f: B ⇒ Option[C])
                (implicit W: AsEv[O]): SfT[A,C,I,Event] =
    sync1(efTo(this))(_.v collect f)((ceb,_) ⇒ ceb.v collect f)

  /** Sequentially combines two signal functions */
  def compose[C,O1[+_]](that: SfT[C,A,O1,I]): SfT[C,B,O1,O] =
    SfT[C,B,O1,O]((rc, r) ⇒ that.run(rc,r) >>= { run(_, r) })(that.MI,MO)

  /** Contravariant mapping */
  def contramap[C](f: C ⇒ A): SfT[C,B,I,O] = compose(SfT.id[C,I] map f)

  /** Counts the number of events this event stream fired and
    * stores the results in a signal
    */
  def count(implicit W: AsEv[O]): SfT[A,Int,I,Id] = scanMap { _ ⇒ 1 }

  /** Returns a signal that only fires an event if its new
    * value is different from its old one
    */
  def distinct[BB>:B](implicit E: Equal[O[BB]]): SfT[A,BB,I,O] =
    sync1O(this)(_.v)(
      (cb,cc) ⇒ if ((cb.v: O[BB]) ≟ cc.v) None else Some(cb.v))

  /** Creates an event stream that fires whenever this signal
    * fires an event.
    *
    * This is similar to `changes` but fires even if the signal's
    * new value is not distinct from the old one. Note that unlike
    * `events` the first event will be fired at `T0` with the
    * signal's initial value.
    */
  def ef(implicit W: AsId[O]): SfT[A,B,I,Event] = {
    def f(c: Change[B]): Event[B] = Once(c.v)

    sync1(sfTo(this))(f)((cb,_) ⇒ f(cb))
  }

  /** Creates an event stream that fires whenever this signal
    * fires an event.
    *
    * Note that unlike with function `ef`, the resulting event stream
    * will skip the signal's initial value.
    */
  //@TODO implement
  def events(implicit W: AsId[O]): SfT[A,B,I,Event] = ??? //ef drop 1

  /** Filters an event stream according to the given predicate */
  def filter(p: B ⇒ Boolean)(implicit W: AsEv[O]): SfT[A,B,I,Event] =
    collect[B] { b ⇒ p(b) option b }

  /** Converts this event stream to a signal with initial value
    * `ini`
    */
  def hold[C>:B](ini: C)(implicit W: AsEv[O]): SfT[A,C,I,Id] = 
    scan[C](ini)((next,_) ⇒ next)

  /** Functor map */
  def map[C](f: B ⇒ C): SfT[A,C,I,O] =
    sync1(this)(_.v map f)((cb,_) ⇒ cb.v map f)

  /** Returns an event stream that fires this signals actual value
    * whenever the given event stream fires
    */
  def on[C,AA<:A](ef: SfT[AA,C,I,Event])
                 (implicit W: AsId[O])
                 : SfT[AA,B,I,Event] = upon(ef)((b,_) ⇒ b)

  /** Accumulates events fired by this event stream in a signal
    *
    * If the original event stream fires its first event `a` at time
    * `T0`, the resulting signal's initial value will be
    * `next(a, ini)`, otherwise it will be just `ini`
    *
    * This is the most fundamental function for accumulating the
    * events of an event stream in a signal. All other functions like
    * `hold`, `scanMap`, and so on can be derived from this one
    *
    * @param ini  the initial value of the resulting signal
    * @param next combines the fired event with the
    *             value accumulated so far
    */
  def scan[C](ini: ⇒ C)(next: (B,C) ⇒ C)(implicit W: AsEv[O])
    : SfT[A,C,I,Id] = {
      def in(cb: Change[Event[B]]) = cb.v.fold(next(_, ini), ini)

      def ne(cb: Change[Event[B]], cc: Change[C]) =
        cb.v.fold(next(_, cc.v), cc.v)

      sync1[A,B,C,I,Event,Id](efTo(this))(in)(ne)
    }

  /** Accumulates events by first transferring them to a value
    * with a Monoid instance
    */
  def scanMap[C:Monoid](f: B ⇒ C)(implicit W: AsEv[O]): SfT[A,C,I,Id] = 
    scan(∅[C])((a,b) ⇒ b ⊹ f(a))

  /** Accumulates events in a container */
  def scanPlus[F[+_]](implicit P: ApplicativePlus[F], W: AsEv[O])
    : SfT[A,F[B],I,Id] = scanMap(_.η[F])(P.monoid, W)

  /** Accumulates the results of a stateful calculation */
  def scanSt[S,C](ini: S)(implicit WS: B <:< State[S,C], W: AsEv[O])
    : SfT[A,(S,C),I,Event] = {
    def sfE = scan[Event[(S,C)]](Never)((s: B, e: Event[(S,C)]) ⇒ 
      e map { s run _._1 } orElse Once(s run ini))

    SfT[A,(S,C),I,Event](sfE.run)
  }

  /** Accumulates the results of a stateful calculation in a signal
    * starting at value `ini`.
    *
    * Note that the stateful calculation is performed purely for its
    * effects on the state and the result is discarded.
    */
  def scanStHold[S,C]
    (ini: S)
    (implicit WS: B <:< State[S,C], W: AsEv[O]): SfT[A,S,I,Id] =
    scanStS[S,C](ini) hold ini

  /** Accumulates the results of a stateful calculation 
    * keeping the state and discarding the result
    */
  def scanStS[S,C]
    (ini: S)
    (implicit WS: B <:< State[S,C], W: AsEv[O]): SfT[A,S,I,Event] =
    scanSt[S,C](ini) map { _._1 }

  /** Accumulates the results of a stateful calculation 
    * discarding the new state
    */
  def scanStV[S,C](ini: S)
    (implicit WS: B <:< State[S,C], W: AsEv[O]): SfT[A,C,I,Event] =
    scanSt[S,C](ini) map { _._2 }

  /** Accumulates events using a Monoid */
  def sum[BB>:B](implicit M: Monoid[BB], W: AsEv[O]): SfT[A,BB,I,Id] = 
    scanMap[BB](identity)

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
  def toE[C,O1[+_]](that: SfT[B,C,Event,O1])
                   (implicit W: AsId[O]): SfT[A,B,I,Id] = 
    sfTo(this) to (SfT.idS[B].ef to that)

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
  def upon[C,D,AA<:A](ef: SfT[AA,C,I,Event])
                     (f: (B,C) ⇒ D)
                     (implicit W: AsId[O]): SfT[AA,D,I,Event] = {
    def g(cb: Change[B], cec: Change[Event[C]]): Option[Event[D]] =
      if (cec.at >= cb.at) Some(cec.v map { f(cb.v,_) } )
      else None

    def ini(cb: Change[B], cec: Change[Event[C]]) = g(cb,cec) | Never

    sync2(sfTo(this),ef)(ini)((cb,cec,ced) ⇒ g(cb,cec) | ced.v)
  }

  /** Alias for `andThen` */
  def >=>[C,O1[+_]](that: SfT[B,C,O,O1]): SfT[A,C,I,O1] = andThen(that)

  /** Alias for `compose` */
  def <=<[C,O1[+_]](that: SfT[C,A,O1,I]): SfT[C,B,O1,O] = compose(that)

  /** Combines two signals with a pure function */
  def <*>[C,D,AA<:A](that: SfT[AA,C,I,O])(f: (B,C) ⇒ D): SfT[AA,D,I,O] =
    sync2(this, that)((cb,cc) ⇒ ^(cb.v, cc.v)(f))((cb,cc,_) ⇒ ^(cb.v, cc.v)(f))

  /** Alias for `contramap`*/
  def ∙ [C](f: C ⇒ A): SfT[C,B,I,O] = contramap(f)

  /** Alias for `syncTo` */
  def -->(out: Out[O[B]]): SfT[A,B,I,O] = syncTo(out)

  /** Alias for `to` */
  def >|>[C,O1[+_]](that: SfT[B,C,O,O1]): SfT[A,B,I,O] = to(that)
}

object SfT extends SfTFunctions {

  type AsId[O[+_]] = O[Any] === Id[Any]

  type AsEv[O[+_]] = O[Any] === Event[Any]

  private def sfTo[A,B,I[+_],O[+_]:AsId](sf: SfT[A,B,I,O])
    : SfT[A,B,I,Id] = sf.asInstanceOf

  private def efTo[A,B,I[+_],O[+_]:AsEv](sf: SfT[A,B,I,O])
    : SfT[A,B,I,Event] = sf.asInstanceOf

  private[dire] def apply[A,B,I[+_]:IdOrEvent,O[+_]:IdOrEvent]
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

  private[dire] def id[A,F[+_]:IdOrEvent]: SfT[A,A,F,F] = SfT((a,_) ⇒ IO(a))

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
  private[dire] def sync2[R,A,B,C,I[+_]:IdOrEvent,O1[+_],O2[+_],O3[+_]:IdOrEvent]
    (sa: SfT[R,A,I,O1], sb: SfT[R,B,I,O2])
    (ini: (Change[O1[A]], Change[O2[B]]) ⇒ O3[C])
    (next: (Change[O1[A]], Change[O2[B]], Change[O3[C]]) ⇒ O3[C])
    : SfT[R,C,I,O3] = sync2O(sa, sb)(ini)((ca,cb,cc) ⇒ isNeverToO(next(ca,cb,cc)))

  private[dire] def sync2O[R,A,B,C,I[+_]:IdOrEvent,O1[+_],O2[+_],O3[+_]:IdOrEvent]
    (sa: SfT[R,A,I,O1], sb: SfT[R,B,I,O2])
    (ini: (Change[O1[A]], Change[O2[B]]) ⇒ O3[C])
    (next: (Change[O1[A]], Change[O2[B]], Change[O3[C]]) ⇒ Option[O3[C]])
    : SfT[R,C,I,O3] =
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
  private[dire] def sync1[R,A,B,I[+_]:IdOrEvent,O1[+_],O2[+_]:IdOrEvent]
    (sa: SfT[R,A,I,O1])
    (ini: Change[O1[A]] ⇒ O2[B])
    (next: (Change[O1[A]], Change[O2[B]]) ⇒ O2[B])
    : SfT[R,B,I,O2] = sync1O(sa)(ini)((ca,cb) ⇒ isNeverToO(next(ca,cb)))

  private[dire] def sync1O[R,A,B,I[+_]:IdOrEvent,O1[+_],O2[+_]:IdOrEvent]
    (sa: SfT[R,A,I,O1])
    (ini: Change[O1[A]] ⇒ O2[B])
    (next: (Change[O1[A]], Change[O2[B]]) ⇒ Option[O2[B]]): SfT[R,B,I,O2] =
    SfT[R,B,I,O2]((rr, r) ⇒ 
      for {
        ra ← sa.run(rr, r)
        rb ← RS.sync1(ra)(ini)(next)
      } yield rb
    )

  private[dire] def cApp[F[+_]:Applicative] =
    Applicative[Change].compose[F]


  private[dire] def isNeverToO[F[+_]:IdOrEvent,A](f: F[A]): Option[F[A]] =
    if (IdOrEvent[F] isNever f) None else Some(f)
}

private[dire] sealed abstract class IdOrEvent[F[+_]](m: Applicative[F])
    extends Applicative[F] {
  def isNever[A](f: F[A]): Boolean
  final def point[A](a: ⇒ A) = m point a
  final def ap[A,B](fa: ⇒ F[A])(f: ⇒ F[A ⇒ B]) = m.ap(fa)(f)
}

private[dire] object IdOrEvent {
  def apply[F[+_]: IdOrEvent]: IdOrEvent[F] = implicitly

  implicit val IdIdOrEvent: IdOrEvent[Id] = new IdOrEvent[Id](Applicative[Id]) {
    def isNever[A](f: Id[A]) = false
  }

  implicit val EventIdOrEvent: IdOrEvent[Event] =
    new IdOrEvent[Event](Applicative[Event]) {
      def isNever[A](f: Event[A]) = f.fold(_ ⇒ false, true)
    }
}

// vim: set ts=2 sw=2 nowrap et:
