package dire.swing

import Swing._
import javax.swing.JScrollPane
import scalaz._, Scalaz._, effect.IO

final case class ScrollPane(peer: JScrollPane)

object ScrollPane {
  def apply[A:Comp](a: A, props: ScrollPane ⇒ IO[Unit]*)
    : IO[ScrollPane] = for {
        res ← IO(new ScrollPane(new JScrollPane(Comp[A] peer a)))
        _   ← res setList props.toList
      } yield res

  implicit val ScrollPaneComponent = new ScrollPaneLike[ScrollPane] {
    def peer(b: ScrollPane) = b.peer
  }

  implicit val ScrollPaneElem: AsSingleElem[ScrollPane] = Elem vhFill { _.peer }
}

// vim: set ts=2 sw=2 et:
