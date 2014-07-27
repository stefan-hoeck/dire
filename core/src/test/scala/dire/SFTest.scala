package dire

import DataSink.buffer
import org.scalacheck._, Prop._
import scalaz._, Scalaz._, effect.IO
//
object SFTest
   extends Properties("SF")
   with dire.control.Runner 
   with SFArbitrary {
  import dire.control.Runner.Events

  //Signal function from Time to A
  type TSF[A] = SF[Time,A]

  val appLaw = Applicative[TSF].applicativeLaw

  val monLaw = Monoid[SFTT].monoidLaw

  val arrLaw = Arrow[SF].categoryLaw

  // the main time signal
  val idTime = SF.id[Time]

  // a second signal that runs asynchronously to time
  // this is used to test combinations of two signals / event streams
  val tickCount = idTime >> (SF ticks 1L count)

  val tickCountC = SF.cached(tickCount, "tickCount")

  implicit def TSFEqual[A:Equal]: Equal[TSF[A]] = new Equal[TSF[A]] {
    def equal(a: TSF[A], b: TSF[A]) = compare(a, b, 100L)(identity)
  }

  // ***             ***//
  // *** Basic tests ***//
  // ***             ***//

  property("idTime") = test100O(idTime)(Some(_))

  property("tickCount") = forAll(Gen choose (20, 100)) { i ⇒ 
    val res = runFor(tickCount, i) collect { case Once(at,v) ⇒ v }

    res ≟ (0 until res.size toList)
  }

  // ***             ***//
  // *** Applicative ***//
  // ***             ***//

  property("basic_functor") =
    test100O(idTime ∘ { 1 + })(t ⇒ Some(t + 1))

  property("functor_identity") = forAll { t: SFTT ⇒ 
    appLaw identity SF.cached(t, "first")
  }

  property("functor_comp") = forAll { t: (Time ⇒ String, String ⇒ Int, SFTT) ⇒  
    appLaw.composite(SF.cached(t._3, "first"), t._1, t._2)
  }

  property("basic_applicative") =
    test100O(idTime ⊛ idTime apply { _ + _ })(t ⇒ Some(t * 2))

  property("applicative_identity") = forAll { t: SFTT ⇒ 
    appLaw identityAp SF.cached(t, "first")
  }

  property("applicative_comp") = forAll {
    p: (Time ⇒ (Time ⇒ String), Time ⇒ (String ⇒ Int), SFTT, SFTT, SFTT) ⇒  
    val sfString = SF.cached(p._3 map p._1, "first")
    val sfInt = SF.cached(p._4 map p._2, "second")

    appLaw.composition(sfInt, sfString, SF.cached(p._5, "third"))
  }

  property("applicative_homomorphism") = forAll { 
    p: (Time ⇒ String, Time) ⇒  
    appLaw.homomorphism(p._1, p._2)
  }

  property("applicative_interchange") = forAll {
    p: (Time ⇒ (Int ⇒ String), Int, SFTT) ⇒  
    appLaw.interchange(SF.cached(p._3 map p._1, "first"), p._2)
  }

  // ***        ***//
  // *** Monoid ***//
  // ***        ***//

  property("basic_monoid_id_left") =
    test100(idTime ⊹ (SF.id[Time] >> SF.never))(identity)

  property("basic_monoid_id_right") =
    test100((SF.id[Time] >> SF.never[Time]) ⊹ idTime)(identity)

  property("monoid_left_identity") = forAll { t: SFTT ⇒ 
    monLaw.leftIdentity(SF.cached(t, "first"))
  }

  property("monoid_right_identity") = forAll { t: SFTT ⇒ 
    monLaw.rightIdentity(SF.cached(t, "first"))
  }

  property("monoid_associative") = forAll { t: (SFTT, SFTT, SFTT) ⇒ 
    val (a, b, c) = t

    monLaw.associative(SF.cached(a, "first"),
                       SF.cached(b, "second"),
                       SF.cached(c, "third"))
  }


  // ***          ***//
  // *** Category ***//
  // ***          ***//

  property("category_right_identity") = forAll { t: SFTT ⇒ 
    arrLaw.rightIdentity(SF.cached(t, "first"))
  }

  property("category_left_identity") = forAll { t: SFTT ⇒ 
    arrLaw.leftIdentity(SF.cached(t, "first"))
  }

  property("compose_associative") = forAll { t: (SFTT, SFTT, SFTT) ⇒ 
    arrLaw.associative(SF.cached(t._1, "first"),
                       SF.cached(t._2, "second"),
                       SF.cached(t._3, "third"))
  }

  property("first_behavior") = {
    //tests that no reactive branch is setup twice
    import collection.mutable.ListBuffer
    val times = new ListBuffer[Long]
    val squares = new ListBuffer[Long]
    val sums = new ListBuffer[Long]

    val sf = SF sf { t: Time ⇒ t * t } to buffer(squares)
    val fst = sf.first[Time] map { case (a,b) ⇒ a + b } to buffer(sums)
    val tot = idTime to buffer(times) map { t ⇒ (t, t) } andThen fst
    
    val res = test100(tot)(_ map { t ⇒ t * t + t })

    res :| "res" &&
    (times.size ≟ 101) :| "times.size" &&
    (squares.toList ≟ times.toList.map{t ⇒ t*t}) :| "squares" &&
    (sums.toList ≟ times.toList.map{t ⇒ t*t + t}) :| "sums" 
  }

  // ***                ***//
  // *** Sync functions ***//
  // ***                ***//

  property("collect") = {
    def pf(l: Long) = (l % 2L == 0L) option l

    test100O(idTime collect { case l if l % 2L == 0L ⇒ l })(pf)
  }

  property("collectO") = {
    val f = (l: Long) ⇒ (l % 2L == 0L) option l

    test100O(idTime collectO f)(f)
  }

  property("contramap") =
    test100O(idTime ∙ { _ + 1L })(x ⇒ Some(x + 1L))

  property("count") =
    test100(idTime.count)(xs ⇒ 1 to xs.size toList)

  property("distinct") = forAll { t: SFTT ⇒ 
    val cached = SF.cached(t, "distinct")
    compare(cached, cached.distinct, 100L)(collectDistinct)
  }

  property("drop") = test100(idTime drop 10)(_ drop 10)

  property("events") = forAll { t: SFTT ⇒ 
    val cached = SF.cached(t, "events")

    def calc(ts: Events[Time]): Events[Time] = ts match {
      case Nil   ⇒ Event.never[Time] :: Nil
      case e::es ⇒ Event.never[Time] :: es
    }

    compare(cached, cached.events, 100L)(calc)
  }

  property("filter") = {
    def even = (l: Long) ⇒ l % 2L == 0L

    test100(idTime filter even)(_ filter even)
  }

  property("grouped") = 
    test100(idTime grouped 7)(_.grouped(7).toList filter { _.size ≟ 7 })

  property("grouped_0") = test100(idTime grouped 0)(_ ⇒ Nil)

  property("groupAsPairs") = test100(idTime.groupAsPairs)(xs ⇒ 
      xs.grouped(2).toList collect { case a::b::Nil ⇒ (a,b) })

  property("groupAsTriples") = test100(idTime.groupAsTriples)(xs ⇒ 
      xs.grouped(3).toList collect { case a::b::c::Nil ⇒ (a,b,c) })

  property("head") = test100(idTime.head)(xs ⇒ List(xs.head))

  property("hold") =
    test100(idTime hold 1000L)(xs ⇒ 1000L :: xs.tail)

  property("io") = {
    var called = false

    val sf = IO { called = true; idTime }

    test100(SF io sf)(identity) && called
  }

  property("once") = forAll { i: Int ⇒ 
    val res = runUntil(SF once i)(i ≟ _)
    
    res ≟ List(Event.never[Int], Event.once(1L, i))
  }

  property("all") = {
    val is = (0 to 10).toList
    val res = runUntil(SF all is)(10 ≟ _)
    
    res ≟ (Event.never[Int] :: is.map { i ⇒ Event.once(i + 1, i) })
  }

  property("on_never") = {
    val on = idTime on (idTime >> SF.never[Time])

    test100(on)(_ ⇒ Nil)
  }

  property("on_const") = {
    val on = idTime on (idTime >> SF.const(100L))

    test100(on)(_ ⇒ List(0L))
  }

  property("on_once") = {
    val on = SF.time on SF.once(100L)

    runUntil(on)(0L <= _).size ≟ 2
  }

  property("or") = {
    val ts = (SF ticks 1L count).events 

    val res = 
      runUntil(ts or ts.map{_.toString})(_ ≟ 100.toString.right)

    val set: Set[Int \/ String] = res flatMap { _.toOption} toSet
    
    (set.size > 100) :| "size" &&
    ((1 to 100).toList ∀ { i ⇒ set(i.toString.right) }) :| "Rights" &&
    ((1 to (set.size - 100)).toList ∀ { i ⇒ set(i.left) }) :| "Lefts"
  }

  property("scan_signal") =
    test100(idTime.scan(0L){ _ - _ })(_.tail.scanLeft(0L){(a,b) ⇒ b - a}) 

  property("scan_event_stream") =
    test100(idTime.events.scan(0L){ _ - _ })(_.tail.scanLeft(0L){(a,b) ⇒ b - a}) 

  property("scanMap") =
    test100(idTime scanMap identity)(_.tail.scanLeft(0L){ _ + _ }) 

  property("scanPlus") =
    test100(idTime.scanPlus[List])(_.tail.scanLeft(List(0L)){(a,b) ⇒ a ++ List(b)}) 

  property("scanSt") = {
    val st = (t: Time) ⇒ State[Int,Time](i ⇒ (i + 1, t))

    test100(idTime map st scanSt 0)(xs ⇒ 
      xs.zipWithIndex map { case (t,i) ⇒ (i + 1, t) })
  }

  property("scanStS") = {
    val st = (t: Time) ⇒ State[Int,Time](i ⇒ (i + 1, t))

    test100(idTime map st scanStS 0)(xs ⇒ 
      xs.zipWithIndex map { case (t,i) ⇒ i + 1})
  }

  property("scanStV") = {
    val st = (t: Time) ⇒ State[Int,Time](i ⇒ (i + 1, t))

    test100(idTime map st scanStV 0)(xs ⇒ 
      xs.zipWithIndex map { case (t,i) ⇒ t})
  }

  property("sliding") = test100(idTime sliding 7){ _ sliding 7 toList }

  property("slidingAsPairs") = test100(idTime.slidingAsPairs)(xs ⇒ 
    xs zip xs.tail)

  property("slidingAsTriples") = test100(idTime.slidingAsTriples)(xs ⇒ 
    xs.sliding(3).toList collect { case a::b::c::Nil ⇒ (a,b,c) })

  property("sum") = test100(idTime.sum)(_.tail.scanLeft(0L){ _ + _}) 

  property("take") = test100(idTime take 10)(_ take 10)

  property("tail") = test100(idTime.tail)(_.tail)

  property("upon_never") = forAll { sf: SFTT ⇒ 
    val sfCached: TSF[Time] = SF.cached(sf, "upon_sf")

    val upon = sfCached.upon(idTime >> SF.never[Time]){ _ + _ }

    compare(sfCached, upon, 100L)(_ ⇒ List(Event.never))
  }

  property("upon_now") = forAll { sf: SFTT ⇒ 
    val sfCached: TSF[Time] = SF.cached(sf, "upon_sf")

    val upon = sfCached.upon(idTime >> SF.const(12L)){ _ + _ }

    def calc(cs: Events[Time]): Events[Time] =
      List(Once(T0, cs.head.fold(_ + 12L, sys.error("What?"))))

    compare(sfCached, upon, 100L)(calc)
  }

  property("upon_once") = {
    val upon = (SF.time upon SF.once(12L)){ _ + _ }

    runUntil(upon)(_ ⇒ true).size ≟ 2
  }

  property("zip") = test100(idTime zip idTime)(ts ⇒ ts zip ts)

  private def collectDistinct(cs: Events[Time]): Events[Time] = {
    val res = new collection.mutable.ListBuffer[Event[Time]]

    def run(rem: Events[Time]): Unit = rem match {
      case (ea@Once(_,va))::Once(_,vb)::rest if(va ≟ vb) ⇒ run(ea :: rest)
      case ea::rest                     ⇒ { res += ea; run(rest) }
      case Nil                          ⇒ ()
    }

    run(cs)

    res.toList
  }

  // ***                  ***//
  // *** Transformer tests ***//
  // ***                  ***//

  property("connectOuts") = {
    val trans: Out[Time] ⇒ Out[Time] = ot ⇒ t ⇒ ot(t * t)

    val res = runUntil(SF.time >=> SF.connectAsync(trans)){ 10000L <= }
    res.flatMap(_.toOption) ≟ (0 to 100 map { t ⇒ t.toLong * t.toLong } toList)
  }

}

// vim: set ts=2 sw=2 et:
