package dire

import SF.EventsOps
import org.scalacheck._, Prop._
import scalaz._, Scalaz._, effect.IO

object SFTest extends Properties("SF") with dire.control.Runner {

  //Signal function from Time to A
  type TSF[+A] = SF[Time,A]

  //Event Stream from Time to A
  type TEF[+A] = EF[Time,A]

  val appLaw = Applicative[TSF].applicativeLaw

  val funLaw = Functor[TEF].functorLaw

  val monLaw = Monoid[TEF[Int]].monoidLaw

  // the main time signal
  val idTime = Arrow[SF].id[Time]

  // a second signal that runs asynchronuously to time
  // this is used to test combinations of two signals / event streams
  val tickCount = (SF ticks 1L count) compose idTime

  val tickCountC = SF.cached(tickCount, "tickCount")

  implicit def EventEqual[A:Equal]: Equal[Event[A]] = new Equal[Event[A]] {
    def equal(a: Event[A], b: Event[A]) = (a,b) match {
      case (Never,Never)              ⇒ true
      case (Once(x),Once(y)) if x ≟ y ⇒ true
      case _                          ⇒ false
    }
  }

  implicit def TSFEqual[A:Equal]: Equal[TSF[A]] = new Equal[TSF[A]] {
    def equal(a: TSF[A], b: TSF[A]) = compare(a, 20L)(b)
  }

  implicit def TEFEqual[A:Equal]: Equal[TEF[A]] = new Equal[TEF[A]] {
    def equal(a: TEF[A], b: TEF[A]) = compare(a, 20L)(b)
  }

  val tGen = Gen choose (1L, 100L)

  //Basic tests
  property("idTime") =
    runFor(idTime, 100L) ≟ (0 to 100 map { i ⇒ Change(i, i: Long) } toList)

  property("tickCount") = forAll(Gen choose (20, 100)) { i ⇒ 
    val res = runFor(tickCount, i) map { _.v }

    res ≟ (0 until res.size toList)
  }

  //Applicative Laws: Signals
  property("functor_identity") = appLaw identity idTime

  property("functor_comp") = forAll { p: (Time ⇒ String, String ⇒ Int) ⇒  
    appLaw.composite(idTime, p._1, p._2)
  }

  property("applicative_identity") = appLaw identityAp idTime

  property("applicative_comp") = forAll {
    p: (Time ⇒ (Time ⇒ String), Time ⇒ (String ⇒ Int)) ⇒  
    val sfString = idTime map p._1
    val sfInt = idTime map p._2

    appLaw.composition(sfInt, sfString, idTime)
  }

  property("applicative_comp_async") = forAll {
    p: (Time ⇒ (Time ⇒ String), Int ⇒ (String ⇒ Int)) ⇒  
    val sfString = idTime map p._1

    //Caching needed, otherwise TWO distinct asynchronous signals
    //will be generated. Those would of course not be equal
    val sfInt = SF.cached(tickCount map p._2, "count")

    appLaw.composition(sfInt, sfString, idTime)
  }

  property("applicative_homomorphism") = forAll { p: (Time ⇒ String, Time) ⇒  
    appLaw.homomorphism(p._1, p._2)
  }

  property("applicative_interchange") = forAll {
    p: (Time ⇒ (Int ⇒ String), Int) ⇒  
    appLaw.interchange(idTime map p._1, p._2)
  }

  //Functor Laws: EventStreams
  property("functor_identity_events") = funLaw identity tickCountC.events

  property("functor_comp_events") = forAll {
    p: (Int ⇒ String, String ⇒ Int) ⇒  

    funLaw.composite(tickCountC.events, p._1, p._2)
  }

  //Monoid Laws: EventStreams
  property("monoid_left_identity") =
    monLaw.leftIdentity(idTime.map(_.toInt).events ⊹ tickCountC.events)

  property("monoid_right_identity") =
    monLaw.rightIdentity(idTime.map(_.toInt).events ⊹ tickCountC.events)

  property("monoid_associative") = forAll { i: Int ⇒ 
    monLaw.associative(idTime map { _.toInt } events,
                       tickCountC.events mapE (i*),
                       SF cached (tickCount, "tickCount2") events)
  }
    
}

// vim: set ts=2 sw=2 et:
