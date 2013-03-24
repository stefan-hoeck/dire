package dire.example

import dire.{SF, Time, Out, EIn, SIn}, SF.EventsOps
import scalaz._, Scalaz._, effect.IO

object UserBob {

  implicit def SMonoid[A:Monoid] = Monoid.liftMonoid[SIn,A]

  private def bob = user("Bob", 992L)

  private def others = List(("Mary", 2113L),
                            ("Percy", 512L),
                            ("Tim", 1135L),
                            ("Gundi", 4012L),
                            ("Troll", 10000L))

  private def user(p: (String,Time)): SIn[Int] = p match {
    case (name, freq) ⇒
      SF cached ((SF ticks freq).count to printAccess(name), name)
  }

  private def calcRatio(bob: Int, all: Int): Double = (bob, all) match {
    case (0,0) ⇒ 0D
    case (b,t) ⇒ b.toDouble / t.toDouble
  }


  private def total = bob ⊹ (others foldMap user)

  private def ratio = ^(bob, total)(calcRatio) to printRatio

  def run = SF.runS(ratio.changes.count)(_ >= 1000)

  private def printAccess(name: String)(count: Int) =
    IO putStr s"$name accessed server (count: $count): "

  private def printRatio(r: Double) = IO putStrLn s"Ratio: $r"
}

// vim: set ts=2 sw=2 et:
