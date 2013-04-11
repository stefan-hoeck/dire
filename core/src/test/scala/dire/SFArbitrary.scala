package dire

import org.scalacheck._ 
import Arbitrary.{arbitrary ⇒ arb}
import scalaz._, Scalaz._

/** Generators for all kinds of reactive graphs */
//trait SFArbitrary {
//  type SFTT = SF[Time,Time]
//
//  type EFTT = EF[Time,Time]
//
//  type SEFTT = SEF[Time,Time]
//
//  type ESFTT = ESF[Time,Time]
//
//  def const(t: Time): SFTT = RF.idS[Time] >> RF.const(t)
//  
//  val idTT: SFTT = Arrow[SF].id[Time]
//
//  val asyncTT: SFTT = RF.idS[Time] >> (RF ticks 1L scan 0L)((_,c) ⇒ c + 1L)
//
//  val square: SFTT = RF.sf[Time,Time] { x ⇒ x * x }
//
//  val inverse: SFTT = RF.sf[Time,Time] { x ⇒ -x }
//
//  val distinct: SFTT = idTT.distinct
//
//  val single: Gen[SFTT] = Gen oneOf (
//    idTT,
//    asyncTT,
//    square,
//    inverse,
//    distinct,
//    arb[Long] map const)
//
//  lazy val composed: Gen[SFTT] = for {
//    a ← single
//    b ← allSF
//  } yield a >>> b
//
//  lazy val applied: Gen[SFTT] = for {
//    a ← single
//    b ← allSF
//  } yield (a <*> b){ _ - _ }
//
//  lazy val mapped: Gen[SFTT] = for {
//    i ← arb[Long]
//    s ← allSF
//  } yield s map (i+)
//
//  lazy val allSF: Gen[SFTT] = Gen oneOf (single, composed, applied, mapped)
//
//  lazy val ef: Gen[SEFTT] = allSF map { _.ef }
//
//  lazy val events: Gen[SEFTT] = allSF map { _.events }
//
//  lazy val changes: Gen[SEFTT] = allSF map { _.changes }
//  
//  lazy val merged = for {
//    a ← Gen oneOf (events, changes)
//    b ← Gen oneOf (events, changes)
//  } yield a ⊹ b
//
//  implicit lazy val sfttArb: Arbitrary[SFTT] = Arbitrary(allSF)
//
//  implicit lazy val sefttArb: Arbitrary[SEFTT] =
//    Arbitrary(Gen oneOf (ef, events, changes, merged))
//
//  implicit lazy val efttArb: Arbitrary[EFTT] = Arbitrary (
//    Arbitrary.arbitrary[SEFTT] map { s ⇒ SF.idE[Time] hold 0L andThen s }
//  )
//
//  implicit lazy val esfttArb: Arbitrary[ESFTT] = Arbitrary (
//    Arbitrary.arbitrary[SFTT] map { s ⇒ SF.idE[Time] hold 0L andThen s }
//  )
//}

// vim: set ts=2 sw=2 et:
