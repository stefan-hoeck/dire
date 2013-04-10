package dire

import org.scalacheck._ 
import Arbitrary.{arbitrary ⇒ arb}
import scalaz._, Scalaz._

/** Generators for all kinds of reactive graphs */
trait SFArbitrary {
  type SFTT = SF[Time,Time]

  type EFTT = SEF[Time,Time]

  def const(t: Time): SFTT = SfT.idS[Time] >> SfT.const(t)
  
  val idTT: SFTT = Arrow[SF].id[Time]

  val asyncTT: SFTT = SfT.idS[Time] >> (SfT ticks 1L scan 0L)((_,c) ⇒ c + 1L)

  val square: SFTT = SfT.sf[Time,Time] { x ⇒ x * x }

  val inverse: SFTT = SfT.sf[Time,Time] { x ⇒ -x }

  val distinct: SFTT = idTT.distinct

  val single: Gen[SFTT] = Gen oneOf (
    idTT,
    asyncTT,
    square,
    inverse,
    distinct,
    arb[Long] map const)

  lazy val composed: Gen[SFTT] = for {
    a ← single
    b ← allSF
  } yield a >>> b

  lazy val applied: Gen[SFTT] = for {
    a ← single
    b ← allSF
  } yield (a <*> b){ _ - _ }

  lazy val mapped: Gen[SFTT] = for {
    i ← arb[Long]
    s ← allSF
  } yield s map (i+)

  lazy val allSF: Gen[SFTT] = Gen oneOf (single, composed, applied, mapped)

  lazy val ef: Gen[EFTT] = allSF map { _.ef }

  lazy val events: Gen[EFTT] = allSF map { _.events }

  lazy val changes: Gen[EFTT] = allSF map { _.changes }
  
  lazy val merged = for {
    a ← Gen oneOf (events, changes)
    b ← Gen oneOf (events, changes)
  } yield a ⊹ b

  implicit lazy val sfttArb: Arbitrary[SFTT] = Arbitrary(allSF)

  implicit lazy val efttArb: Arbitrary[EFTT] =
    Arbitrary(Gen oneOf (ef, events, changes, merged))
}

// vim: set ts=2 sw=2 et:
