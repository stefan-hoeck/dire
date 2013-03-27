package dire

import SF.EventsOps
import org.scalacheck._ 
import Arbitrary.{arbitrary ⇒ arb}
import scalaz._, Scalaz._

/** Generators for all kinds of reactive graphs */
trait SFArbitrary {
  type SFTT = SF[Time,Time]

  type EFTT = EF[Time,Time]

  def const(t: Time): SFTT = SF const t
  
  val idTT: SFTT = Arrow[SF].id[Time]

  val asyncTT: SFTT = (SF ticks 1L scan 0L)((_,c) ⇒ c + 1L)

  val square: SFTT = Arrow[SF].arr[Time,Time] { x ⇒ x * x }

  val inverse: SFTT = Arrow[SF].arr[Time,Time] { x ⇒ -x }

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

  lazy val events: Gen[EFTT] = allSF map { _.events }

  lazy val changes: Gen[EFTT] = allSF map { _.changes }
  
  lazy val merged = for {
    a ← Gen oneOf (events, changes)
    b ← Gen oneOf (events, changes)
  } yield a ⊹ b

  implicit lazy val sfttArb: Arbitrary[SFTT] = Arbitrary(allSF)

  implicit lazy val efttArb: Arbitrary[EFTT] =
    Arbitrary(Gen oneOf (events, changes, merged))
}

// vim: set ts=2 sw=2 et:
