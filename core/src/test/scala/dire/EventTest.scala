package dire

//import org.scalacheck._, Prop._, Arbitrary.{arbitrary ⇒ arb}
//import scalaz._, Scalaz._, scalacheck.{ScalazProperties ⇒ SP}
//import scalaz.scalacheck.ScalazArbitrary._
//import scalaz.scalacheck.ScalaCheckBinding._
//
//object EventTest extends Properties("Event") {
//  implicit def EArb[A:Arbitrary]: Arbitrary[Event[A]] =
//    Arbitrary(Gen.frequency[Event[A]]((5, arb[A] map Once.apply), (1, Never)))
//
//  implicit def EEqual[A]: Equal[Event[A]] = Equal.equalA
//
//  implicit val EventMonad: Monad[Event] = new Monad[Event] {
//    def point[A](a: ⇒ A) = Once(a)
//    def bind[A,B](ea: Event[A])(f: A ⇒ Event[B]) = ea flatMap f
//  }
//
//  property("equal") = SP.equal.laws[Event[Int]]
//
//  property("monad") = SP.monad.laws[Event]
//}

// vim: set ts=2 sw=2 et:
