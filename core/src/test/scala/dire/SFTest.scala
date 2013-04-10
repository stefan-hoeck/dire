package dire

import org.scalacheck._, Prop._
import scalaz._, Scalaz._, effect.IO

object SFTest
   extends Properties("SF")
   with dire.control.Runner 
   with SFArbitrary {

  //Signal function from Time to A
  type TSF[+A] = SF[Time,A]

  //Event Stream from Time to A
  type TEF[+A] = SEF[Time,A]

  val appLaw = Applicative[TSF].applicativeLaw

  val appLawEF = Applicative[TEF].applicativeLaw

  val monLaw = Monoid[SEFTT].monoidLaw

  val arrLaw = Arrow[SF].categoryLaw

  val arrLawEF = Arrow[EF].categoryLaw

  // the main time signal
  val idTime = SF.idS[Time]

  // a second signal that runs asynchronuously to time
  // this is used to test combinations of two signals / event streams
  val tickCount = idTime >> (SF ticks 1L count)

  val tickCountC = SF.cached(tickCount, "tickCount")

  implicit def TSFEqual[A:Equal]: Equal[TSF[A]] = new Equal[TSF[A]] {
    def equal(a: TSF[A], b: TSF[A]) = compare(a, b, 100L)(identity)
  }

  implicit def TEFEqual[A:Equal]: Equal[TEF[A]] = new Equal[TEF[A]] {
    def equal(a: TEF[A], b: TEF[A]) = compare(a, b, 100L)(identity)
  }

  implicit val EFTTEqual: Equal[EFTT] = new Equal[EFTT] {
    def equal(a: EFTT, b: EFTT) = compare(
      SF.idS[Time].ef andThen a, SF.idS[Time].ef andThen b, 100L)(identity)
  }

  // ***             ***//
  // *** Basic tests ***//
  // ***             ***//

  property("idTime") = test100(idTime)(Some(_))

  property("tickCount") = forAll(Gen choose (20, 100)) { i ⇒ 
    val res = runFor(tickCount, i) map { _.v }

    res ≟ (0 until res.size toList)
  }

  // ***                     ***//
  // *** Applicative Signals ***//
  // ***                     ***//

  property("basic_functor") =
    test100(idTime ∘ { 1 + })(t ⇒ Some(t + 1))

  property("functor_identity") = forAll { t: SFTT ⇒ 
    appLaw identity SF.cached(t, "first")
  }

  property("functor_comp") = forAll { t: (Time ⇒ String, String ⇒ Int, SFTT) ⇒  
    appLaw.composite(SF.cached(t._3, "first"), t._1, t._2)
  }

  property("basic_applicative") =
    test100(idTime ⊛ idTime apply { _ + _ })(t ⇒ Some(t * 2))

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

  // ***                           ***//
  // *** Applicative event streams ***//
  // ***                           ***//

  property("basic_functor_events") =
    test100(idTime.ef map { 1 + })(t ⇒ Some(Once(t + 1)))

  property("functor_identity") = forAll { t: SEFTT ⇒ 
    appLawEF identity SF.cached(t, "first")
  }

  property("functor_comp") = forAll { t: (Time ⇒ String, String ⇒ Int, SEFTT) ⇒  
    appLawEF.composite(SF.cached(t._3, "first"), t._1, t._2)
  }

  property("basic_applicative") =
    test100((idTime.ef <*> idTime.ef){ _ + _ })(t ⇒ Some(Once(t * 2)))

  property("applicative_identity") = forAll { t: SEFTT ⇒ 
    appLawEF identityAp SF.cached(t, "first")
  }

  property("applicative_comp") = forAll {
    p: (Time ⇒ (Time ⇒ String), Time ⇒ (String ⇒ Int), SEFTT, SEFTT, SEFTT) ⇒  
    val sfString = SF.cached(p._3 map p._1, "first")
    val sfInt = SF.cached(p._4 map p._2, "second")

    appLawEF.composition(sfInt, sfString, SF.cached(p._5, "third"))
  }

  property("applicative_homomorphism") = forAll { 
    p: (Time ⇒ String, Time) ⇒  
    appLawEF.homomorphism(p._1, p._2)
  }

  property("applicative_interchange") = forAll {
    p: (Time ⇒ (Int ⇒ String), Int, SEFTT) ⇒  
    appLawEF.interchange(SF.cached(p._3 map p._1, "first"), p._2)
  }

  // ***                      ***//
  // *** Monoid event streams ***//
  // ***                      ***//

  property("basic_monoid_id_left") =
    test100(idTime.ef ⊹ (SF.idS[Time] >> SF.never))(t ⇒ Some(Once(t)))

  property("basic_monoid_id_right") =
    test100((SF.idS[Time] >> SF.never[Time]) ⊹ idTime.ef)(t ⇒ Some(Once(t)))

  property("monoid_left_identity") = forAll { t: SEFTT ⇒ 
    monLaw.leftIdentity(SF.cached(t, "first"))
  }

  property("monoid_right_identity") = forAll { t: SEFTT ⇒ 
    monLaw.rightIdentity(SF.cached(t, "first"))
  }

  property("monoid_associative") = forAll { t: (SEFTT, SEFTT, SEFTT) ⇒ 
    val (a, b, c) = t

    monLaw.associative(SF.cached(a, "first"),
                       SF.cached(b, "second"),
                       SF.cached(c, "third"))
  }


  // ***                  ***//
  // *** Category signals ***//
  // ***                  ***//

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

  // ***                        ***//
  // *** Category event streams ***//
  // ***                        ***//

  property("category_right_identity_ef") = forAll { t: EFTT ⇒ 
    arrLawEF.rightIdentity(SF.cached(t, "first"))
  }

  property("category_left_identity_ef") = forAll { t: EFTT ⇒ 
    arrLawEF.leftIdentity(SF.cached(t, "first"))
  }

  property("compose_associative_ef") = forAll { t: (EFTT, EFTT, EFTT) ⇒ 
    arrLawEF.associative(SF.cached(t._1, "first"),
                         SF.cached(t._2, "second"),
                         SF.cached(t._3, "third"))
  }
    
  //Once
  property("once") = forAll { i: Int ⇒ 
    val res = runUntil(SF once i)(i ≟ _)
    
    res ≟ List(Change(T0, Never), Change(1L, Once(i)))
  }

  //distinct
  property("distinct") = forAll { t: SFTT ⇒ 
    val cached = SF.cached(t, "distinct")
    compare(cached, cached.distinct, 100L)(collectDistinct)
  }

  //changes
  property("changes") = forAll { t: SFTT ⇒ 
    val cached = SF.cached(t, "changes")
    compare(cached, cached.changes, 100L){
      ts ⇒ collectDistinct(ts) map { _ map Once.apply }
    }
  }

  //ef
  property("ef") = forAll { t: SFTT ⇒ 
    val cached = SF.cached(t, "ef")
    compare(cached, cached.ef, 100L)(_ map { _ map Once.apply })
  }

  //events
  property("events") = forAll { t: SFTT ⇒ 
    val cached = SF.cached(t, "events")

    def calc(ts: Changes[Time]): Changes[Event[Time]] = ts match {
      case Nil   ⇒ Change(T0,Never) :: Nil
      case c::cs ⇒ Change(T0,Never) :: (cs map { _ map Once.apply })
    }

    compare(cached, cached.events, 100L)(calc)
  }

  //upon
  property("upon_never") = forAll { sf: SFTT ⇒ 
    val sfCached: TSF[Time] = SF.cached(sf, "upon_sf")

    val upon = sfCached.upon[Time,Time,Time](
      idTime >> SF.never[Time]){ _ + _ }

    compare(sfCached, upon, 100L)(_ ⇒ List(Change(0L, Never)))
  }

  property("upon_now") = forAll { sf: SFTT ⇒ 
    val sfCached: TSF[Time] = SF.cached(sf, "upon_sf")

    val upon = sfCached.upon[Time,Time,Time](
      idTime >> SF.now(12L)){ _ + _ }

    def calc(cs: Changes[Time]): Changes[Event[Time]] =
      List(Change(T0, Once(cs.head.v + 12L)))

    compare(sfCached, upon, 100L)(calc)
  }

  property("upon_once") = forAll { sf: SFTT ⇒ 
    val upon = (SF.time andThen sf upon SF.once(12L)){ _ + _ }

    runUntil(upon)(_ ⇒ true).size ≟ 2
  }
  
  type Changes[+A] = List[Change[A]]

  private def collectDistinct(cs: Changes[Time]): Changes[Time] = {
    val res = new collection.mutable.ListBuffer[Change[Time]]

    def run(rem: Changes[Time]): Unit = rem match {
      case ca::cb::rest if(ca.v ≟ cb.v) ⇒ run(ca::rest)
      case ca::rest                     ⇒ { res += ca; run(rest) }
      case Nil                          ⇒ ()
    }

    run(cs)

    res.toList
  }
}

// vim: set ts=2 sw=2 et:
