package dire.example

import dire._, SF.EFOps
import scalaz._, Scalaz._, effect.IO

object UserBob {

  def run = SF.run(ratio.events.count)(_ >= 10000)

  implicit def SMonoid[A:Monoid] = Monoid.liftMonoid[SIn,A]

  private def bob = user("Bob", 992L, 0.995D)

  private def others = List(("Troll", 0L, 0.993D),
                            ("Percy", 512L, 0.991D),
                            ("Tim", 1135L, 0.9925D),
                            ("Gundi", 4012L, 0.998D),
                            ("Mary", 2113L, 0.992D))

  private def user(p: (String,Long,Double)): SIn[Int] = p match {
    case (name, seed, freq) ⇒ {
      val accesses = Random noise (1000L, seed) filter { freq <= } count

      SF cached (accesses --> printAccess(name), name)
    }
  }

  private def calcRatio(bob: Int, all: Int): Double = (bob, all) match {
    case (0,0) ⇒ 0D
    case (b,t) ⇒ b.toDouble / t.toDouble
  }


  private def total: SIn[Int] = (bob ⊹ others.foldMap(user)) --> printTotal

  private def ratio = ^(bob, total)(calcRatio) --> printRatio

  private def printAccess(name: String)(count: Int) =
    IO putStr s"$name accessed server (count: $count); "

  private def printRatio(d: Double) = IO putStrLn s"Ratio: $d"

  private def printTotal(i: Int) = IO putStr s"Total: $i; "
}

// vim: set ts=2 sw=2 et:
