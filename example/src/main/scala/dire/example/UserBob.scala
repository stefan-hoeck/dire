package dire.example

import dire._, SF.EventsOps
import scalaz._, Scalaz._, effect.IO

object UserBob {

  implicit def SMonoid[A:Monoid] = Monoid.liftMonoid[SIn,A]

  private def bob = user("Bob", 992L, 0.95D)

  private def others = List(("Troll", 0L, 0.3D))
                         //   ("Percy", 512L, 0.1D),
                         //   ("Tim", 1135L, 0.25D),
                         //   ("Gundi", 4012L, 0.8D),
                         //   ("Mary", 2113L, 0.2D))

  private def user(p: (String,Long,Double)): SIn[Int] = p match {
    case (name, seed, freq) ⇒ {
      val accesses = Random noise (100000L, seed) filter { _ >= freq } count

      SF cached (accesses changeTo printAccess(name), name)
    }
  }

  private def calcRatio(bob: Int, all: Int): Double = (bob, all) match {
    case (0,0) ⇒ 0D
    case (b,t) ⇒ b.toDouble / t.toDouble
  }


  private def total = bob ⊹ (others foldMap user)

  private def ratio = ^(bob, total)(calcRatio) changeTo printRatio

  def run = SF.runS(ratio.changes.count)(_ >= 1000)

  private def printAccess(name: String)(c: Change[Int]) =
    IO putStrLn s"$name accessed server (count: ${c.v}) at time ${c.at}: "

  private def printRatio(c: Change[Double]) = IO putStrLn s"Ratio: ${c.v} at time ${c.at}"
}

// vim: set ts=2 sw=2 et:
