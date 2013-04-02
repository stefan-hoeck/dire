package dire.example

import dire._, SF.{EFOps, const}
import dire.swing.{Elem, SwingApp, FrameV}
import dire.swing.Animation._
import math.{Pi, sin, cos}
import java.awt.Color
import scalaz._, Scalaz._

object Animation extends SwingApp {

  implicit def SMonoid[A:Monoid] = Monoid.liftMonoid[SIn,A]

  def behavior = for {
    scene ← Scene()
    sf    = (c1 ⊹ c2 ⊹ c3) toSink scene
  } yield (Elem(scene) setDim (1000, 600), sf >>> const(FrameV("Animation")))

  val ticks = EF ticks 10000L count

  // sine wave:
  // f: Frequency [Hz]
  // a: amplitude [Pixels]
  // offset [Pixels]
  // phi: phase [radians]
  def wave(f: Double, a: Double, offset: Double, phi: Double = 0D) =
    ticks map { t ⇒ sin(t * 2 * Pi * f / 100 + phi) * a + offset }

  def c1: SIn[Shape] = ^(
    wave(1, 100, 200),
    wave(1, 100, 200, Pi / 2)
  ) { (x,y) ⇒ Circle((x,y), 50, red) }

  def c2: SIn[Shape] =
    wave(0.2, 300, 500) ∘ { x ⇒ Circle((x, 75.0), 100, green) }

  def c3: SIn[Shape]= ^(
    wave(0.5, 200, 200, Pi / 2),
    wave(0.5, 200, 0)
  ){ (y,r) ⇒ Circle((420.0, y), r.abs.toInt, blue) }

  val green = Color.GREEN

  val red = Color.RED

  val blue = Color.BLUE
}

// vim: set ts=2 sw=2 et:
