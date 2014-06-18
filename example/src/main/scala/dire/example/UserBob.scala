package dire.example

import dire._
import scalaz._, Scalaz._, scalaz.effect.{SafeApp, IO}

/** Simulation of asynchronous client accesses to a server */
object UserBob extends SafeApp {

  implicit def SMonoid[A:Monoid] = Monoid.liftMonoid[SIn,A]

  //number of user accesses after which the application stops
  private val stopAfter = 100

  //increase this number to reduce the number of noise events that
  //lead to a server access.
  private val slowdownFactor = 1000L

  //a user consists of three parameters: a name, a seed for the random
  //number generator, and a server access rate, which is a number
  //between 0 and 1. higher numbers means more frequent server
  //accesses
  type UserData = (String, Long, Double)

  private val bob = ("Bob", -992L, 0.5)

  private val others = List(("Troll", 0L, 0.7),
                            ("Percy", 512L, 0.9),
                            ("Tim", 1135L, 0.75),
                            ("Gundi", 4012L, 0.2),
                            ("Mary", 2113L, 0.8))

  // scalaz.effect.SafeApp entry point
  override def runc = SF.run(ratio.events.count)(_ >= stopAfter)

  private def user(d: UserData): SIn[Int] = d match {
    case (name, seed, rate) ⇒ {
      val freq = 1.0 - rate / slowdownFactor
      val accesses = Random noise seed filter { freq <= } count

      SF.cached(accesses --> printAccess(name), name)
    }
  }

  private def calcRatio(bob: Int, all: Int): Double = (bob, all) match {
    case (0,0) ⇒ 0.0
    case (b,t) ⇒ b.toDouble / t.toDouble
  }

  private def total: SIn[Int] = (bob :: others).foldMap(user) --> printTotal

  private def ratio = ^(user(bob), total)(calcRatio) --> printRatio

  private def printAccess(name: String)(count: Int) =
    IO putStr s"$name accessed server (count: $count); "

  private def printRatio(d: Double) = IO putStrLn s"Ratio: $d"

  private def printTotal(i: Int) = IO putStr s"Total: $i; "
}

// vim: set ts=2 sw=2 et:
