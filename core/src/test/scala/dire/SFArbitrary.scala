package dire

import org.scalacheck._ 
import Arbitrary.{arbitrary ⇒ arb}
import scalaz._, Scalaz._

/** Generators for all kinds of reactive graphs */
trait SFArbitrary {
  type SFTT = SF[Time,Time]

  def const(t: Time): SFTT = SF.id[Time] >> SF.const(t)
  
  val idTT: SFTT = SF.id[Time]

  val asyncTT: SFTT = SF.id[Time] >> (SF ticks 1L scan 0L)((_,c) ⇒ c + 1L)

  val square: SFTT = SF.sf[Time,Time] { x ⇒ x * x }

  val inverse: SFTT = SF.sf[Time,Time] { x ⇒ -x }

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

  lazy val merged: Gen[SFTT] = for {
    a ← single
    s ← allSF
  } yield a merge s

  lazy val allSF: Gen[SFTT] =
    Gen oneOf (single, composed, applied, mapped, merged)

  implicit lazy val sfttArb: Arbitrary[SFTT] = Arbitrary(allSF)
}

// vim: set ts=2 sw=2 et:
