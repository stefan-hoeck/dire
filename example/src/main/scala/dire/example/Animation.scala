package dire.example

import dire._, SF.{EFOps, const}
import dire.swing.{Elem, SwingApp, FrameV}
import dire.swing.Animation._
import math.{Pi, sin, cos}
import java.awt.Color._
import scalaz._, Scalaz._

/** A combination of simple animations
  *
  * To run, modify [[dire.example.Main]] like so:
  *
  * `def runc = Animation.run`
  */
object Animation extends SwingApp {

  implicit def SMonoid[A:Monoid] = Monoid.liftMonoid[SIn,A]

  def behavior = for {
    scene ← Scene()
    sf    = (s1 ⊹ s2 ⊹ s3 ⊹ s4) toSink scene
  } yield (Elem(scene) setDim (1000, 600), sf >>> const(FrameV("Animation")))

  val ticks = SF.cached(EF ticks 10000L count, "ticks")

  // sine wave:
  // f: Frequency [Hz]
  // a: amplitude [Pixels]
  // offset [Pixels]
  // phi: phase [radians]
  def wave(f: Double, a: Double, offset: Double, phi: Double = 0D) =
    ticks map { t ⇒ sin(t * 2 * Pi * f / 100 + phi) * a + offset }

  def s1: SIn[Shape] = ^(
    wave(0.7, 100, 200),
    wave(0.7, 100, 200, Pi / 2)
  ) { circle(_, _, 50, RED) }

  def s2: SIn[Shape] =
    wave(0.2, 300, 500) ∘ { square(_, 75.0, 100, GREEN) }

  def s3: SIn[Shape]= ^(
    wave(0.5, 200, 200, Pi / 2),
    wave(0.5, 100, 150)
  ){ (y,r) ⇒ circle(420.0, y, r.toInt, BLUE) }

  def s4: SIn[Shape]= ^(
    wave(0.3, 300, 400, Pi / 2),
    wave(0.3, 200, 0)
  ){ (y,r) ⇒ square(y, y, r.abs.toInt, YELLOW) }
}

// vim: set ts=2 sw=2 et:
