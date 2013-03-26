package dire.example

import dire._, SF.EventsOps
import scalaz._, Scalaz._, effect.IO

object UserBob {

  def run = SF.runS(ratio.events.count)(_ >= 100)

  implicit def SMonoid[A:Monoid] = Monoid.liftMonoid[SIn,A]

  private def bob = user("Bob", 992L, 0.95D)

  private def others = List(("Troll", 0L, 0.93D),
                            ("Percy", 512L, 0.91D),
                            ("Tim", 1135L, 0.925D),
                            ("Gundi", 4012L, 0.98D),
                            ("Mary", 2113L, 0.92D))

  private def user(p: (String,Long,Double)): SIn[Int] = p match {
    case (name, seed, freq) ⇒ {
      val accesses = Random noise (1000000L, seed) filter { freq <= } count

      SF cached (accesses to printAccess(name), name)
    }
  }

  private def calcRatio(bob: Int, all: Int): Double = (bob, all) match {
    case (0,0) ⇒ 0D
    case (b,t) ⇒ b.toDouble / t.toDouble
  }


  private def total: SIn[Int] = (bob ⊹ others.foldMap(user)) to printTotal

  private def ratio = ^(bob, total)(calcRatio) to printRatio

  private def printAccess(name: String)(count: Int) =
    IO putStr s"$name accessed server (count: $count); "

  private def printRatio(d: Double) = IO putStrLn s"Ratio: $d"

  private def printTotal(i: Int) = IO putStr s"Total: $i; "
}

// vim: set ts=2 sw=2 et:
