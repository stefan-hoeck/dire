package dire

import SF.EventsOps
import org.scalacheck._, Prop._
import scalaz._, Scalaz._, effect.IO

object SFTest
   extends Properties("SF")
   with dire.control.Runner 
   with SFArbitrary {

  //Signal function from Time to A
  type TSF[+A] = SF[Time,A]

  //Event Stream from Time to A
  type TEF[+A] = EF[Time,A]

  val appLaw = Applicative[TSF].applicativeLaw

  val funLaw = Functor[TEF].functorLaw

  val monLaw = Monoid[EFTT].monoidLaw

  val arrLaw = Arrow[SF].categoryLaw

  // the main time signal
  val idTime = Arrow[SF].id[Time]

  // a second signal that runs asynchronuously to time
  // this is used to test combinations of two signals / event streams
  val tickCount = (SF ticks 1L count) compose idTime

  val tickCountC = SF.cached(tickCount, "tickCount")

  implicit def TSFEqual[A:Equal]: Equal[TSF[A]] = new Equal[TSF[A]] {
    def equal(a: TSF[A], b: TSF[A]) = compare(a, 100L)(b)
  }

  //Basic tests
  property("idTime") =
    runFor(idTime, 100L) ≟ (0 to 100 map { i ⇒ Change(i, i: Long) } toList)

  property("tickCount") = forAll(Gen choose (20, 100)) { i ⇒ 
    val res = runFor(tickCount, i) map { _.v }

    res ≟ (0 until res.size toList)
  }

  //Applicative Laws: Signals
  property("functor_identity") = forAll { t: SFTT ⇒ 
    appLaw identity SF.cached(t, "first")
  }

  property("functor_comp") = forAll { t: (Time ⇒ String, String ⇒ Int, SFTT) ⇒  
    appLaw.composite(SF.cached(t._3, "first"), t._1, t._2)
  }

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

  //Functor Laws: EventStreams
  property("functor_identity_events") = forAll { t: EFTT ⇒ 
    funLaw identity SF.cached(t, "first")
  }

  property("functor_comp_events") = forAll {
    p: (Time ⇒ String, String ⇒ Int, EFTT) ⇒  

    funLaw.composite(SF.cached(p._3, "first"), p._1, p._2)
  }

  //Monoid Laws: EventStreams
  property("monoid_left_identity") = forAll { t: EFTT ⇒ 
    monLaw.leftIdentity(SF.cached(t, "first"))
  }

  property("monoid_right_identity") = forAll { t: EFTT ⇒ 
    monLaw.rightIdentity(SF.cached(t, "first"))
  }

  property("monoid_associative") = forAll { t: (EFTT, EFTT, EFTT) ⇒ 
    val (a, b, c) = t

    monLaw.associative(SF.cached(a, "first"),
                       SF.cached(b, "second"),
                       SF.cached(c, "third"))
  }

  //Category Laws
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
    val res = runUntil[Event[Int]](SF once i, Once(i) ≟ _)
    
    res ≟ List(Change(T0, Never), Change(1L, Once(i)))
  }
}

// vim: set ts=2 sw=2 et:
