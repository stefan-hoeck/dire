package dire

import org.scalacheck._, Prop._
import scalaz._, Scalaz._

object SFTest extends Properties("SF") with dire.control.Runner {

  //Signal function from Time to A
  type TSF[+A] = SF[Time,A]
  val appLaw = Applicative[TSF].applicativeLaw
  val idTime = Arrow[SF].id[Time]

  implicit def TSFEqual[A:Equal]: Equal[TSF[A]] = new Equal[TSF[A]] {
    def equal(a: TSF[A], b: TSF[A]) = compare(a, 20L)(b)
  }

  val tGen = Gen choose (1L, 100L)

  //Basic tests
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

  property("applicative_homomorphism") = forAll { p: (Time ⇒ String, Time) ⇒  
    appLaw.homomorphism(p._1, p._2)
  }

  property("applicative_interchange") = forAll {
    p: (Time ⇒ (Int ⇒ String), Int) ⇒  
    appLaw.interchange(idTime map p._1, p._2)
  }
}

// vim: set ts=2 sw=2 et:
