package dire

import org.scalacheck._ 
import Arbitrary.{arbitrary ⇒ arb}
import scalaz._, Scalaz._

/** Generators for all kinds of reactive graphs */
trait SFArbitrary {
  val MaxDepth = 5

  type SFTT = SF[Time,Time]

  def const(t: Time): SFTT = SF.id[Time] >> SF.const(t)
  
  val idTT: SFTT = SF.id[Time]

  val asyncTT: SFTT = SF.id[Time] >> (SF ticks 1L scan 0L)((_,c) ⇒ c + 1L)

  val square: SFTT = SF.sf[Time,Time] { x ⇒ x * x }

  val inverse: SFTT = SF.sf[Time,Time] { x ⇒ -x }

  val distinct: SFTT = idTT.distinct

  val single: Gen[SFTT] = Gen.oneOf[SFTT](
    idTT: Gen[SFTT],
    asyncTT: Gen[SFTT],
    square: Gen[SFTT],
    inverse: Gen[SFTT],
    distinct: Gen[SFTT],
    arb[Long] map const)

  def composed(d: Int): Gen[SFTT] = for {
    a ← single
    b ← allSF(d)
  } yield a >>> b

  def applied(d: Int): Gen[SFTT] = for {
    a ← single
    b ← allSF(d)
  } yield (a <*> b){ _ - _ }

  def mapped(d: Int): Gen[SFTT] = for {
    i ← arb[Long]
    s ← allSF(d)
  } yield s map (i+)

  def merged(d: Int): Gen[SFTT] = for {
    a ← single
    s ← allSF(d)
  } yield a merge s

  def allSF(d: Int): Gen[SFTT] = 
    if (d >= MaxDepth) single
    else Gen oneOf (single, composed(d+1), applied(d+1), mapped(d+1), merged(d+1))

  implicit lazy val sfttArb: Arbitrary[SFTT] = Arbitrary(allSF(0))
}

// vim: set ts=2 sw=2 et:
