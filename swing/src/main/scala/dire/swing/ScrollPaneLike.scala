package dire.swing

import javax.swing.JScrollPane
import javax.swing.ScrollPaneConstants._

trait ScrollPaneLike[-A] extends Component[A] {
  def peer(a: A): JScrollPane

  final def hScrollBarPolicy(a: A): Sink[HScrollBarPolicy] =
    sink(p ⇒ peer(a).setHorizontalScrollBarPolicy(p.v))

  final def viewportBorder(a: A): Sink[Border] =
    sink(b ⇒ peer(a).setViewportBorder(b.jborder))

  final def vScrollBarPolicy(a: A): Sink[VScrollBarPolicy] =
    sink(p ⇒ peer(a).setVerticalScrollBarPolicy(p.v))

  final def wheelScrollingEnabled(a: A): Sink[Boolean] =
    sink(peer(a).setWheelScrollingEnabled)
}

object ScrollPaneLike {
  def apply[A:ScrollPaneLike]: ScrollPaneLike[A] = implicitly

  implicit def ScrollPaneIso[B] = new (ScrollPaneLike ~>> JScrollPane) {
    def apply[A](f: ScrollPaneLike[A], a: A) = f peer a
  }
}

sealed abstract class HScrollBarPolicy(val v: Int)

object HScrollBarPolicy {
  private def get(v: Int): HScrollBarPolicy = new HScrollBarPolicy(v){}

  final val Alyways = get(HORIZONTAL_SCROLLBAR_ALWAYS)
  final val Never = get(HORIZONTAL_SCROLLBAR_NEVER)
  final val AsNeeded = get(HORIZONTAL_SCROLLBAR_AS_NEEDED)
}

sealed abstract class VScrollBarPolicy(val v: Int)

object VScrollBarPolicy {
  private def get(v: Int): VScrollBarPolicy = new VScrollBarPolicy(v){}

  final val Alyways = get(VERTICAL_SCROLLBAR_ALWAYS)
  final val Never = get(VERTICAL_SCROLLBAR_NEVER)
  final val AsNeeded = get(VERTICAL_SCROLLBAR_AS_NEEDED)
}

// vim: set ts=2 sw=2 et:
