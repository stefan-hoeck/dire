package dire

import org.scalacheck._, Prop._
import scalaz._, Scalaz._, effect.IO
//
object SFTest
   extends Properties("SF")
   with dire.control.Runner 
   with SFArbitrary {
  import dire.control.Runner.Events

  //Signal function from Time to A
  type TSF[+A] = SF[Time,A]

  val appLaw = Applicative[TSF].applicativeLaw

  val monLaw = Monoid[SFTT].monoidLaw

  val arrLaw = Arrow[SF].categoryLaw

  // the main time signal
  val idTime = SF.id[Time]

  // a second signal that runs asynchronuously to time
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

  //Once
  property("once") = forAll { i: Int ⇒ 
    val res = runUntil(SF once i)(i ≟ _)
    
    res ≟ List(Never, Once(1L, i))
  }

  //distinct
  property("distinct") = forAll { t: SFTT ⇒ 
    val cached = SF.cached(t, "distinct")
    compare(cached, cached.distinct, 100L)(collectDistinct)
  }

  //events
  property("events") = forAll { t: SFTT ⇒ 
    val cached = SF.cached(t, "events")

    def calc(ts: Events[Time]): Events[Time] = ts match {
      case Nil   ⇒ Never :: Nil
      case e::es ⇒ Never :: es
    }

    compare(cached, cached.events, 100L)(calc)
  }

  //upon
  property("upon_never") = forAll { sf: SFTT ⇒ 
    val sfCached: TSF[Time] = SF.cached(sf, "upon_sf")

    val upon = sfCached.upon[Time,Time,Time](
      idTime >> SF.never[Time]){ _ + _ }

    compare(sfCached, upon, 100L)(_ ⇒ List(Never))
  }

  property("upon_now") = forAll { sf: SFTT ⇒ 
    val sfCached: TSF[Time] = SF.cached(sf, "upon_sf")

    val upon = sfCached.upon[Time,Time,Time](
      idTime >> SF.const(12L)){ _ + _ }

    def calc(cs: Events[Time]): Events[Time] =
      List(Once(T0, cs.head.fold(_ + 12L, sys.error("What?"))))

    compare(sfCached, upon, 100L)(calc)
  }

  property("upon_once") = {
    val upon = (SF.time upon SF.once(12L)){ _ + _ }

    runUntil(upon)(_ ⇒ true).size ≟ 2
  }

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
  // *** Trasformer tests ***//
  // ***                  ***//

  property("connectOuts") = {
    val trans: Out[Time] ⇒ Out[Time] = ot ⇒ t ⇒ ot(t * t)

    val res = runUntil(SF.time >=> SF.connectOuts(trans, None)){ 10000L <= }
    res.flatMap(_.toOption) ≟ (0 to 100 map { t ⇒ t.toLong * t.toLong } toList)
  }

}

// vim: set ts=2 sw=2 et:
