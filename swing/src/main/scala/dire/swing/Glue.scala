package dire.swing

import java.awt.{Component ⇒ AComponent}
import javax.swing.Box
import scalaz.effect.IO

final class Glue(val peer: AComponent)

object Glue {
  def vertical: IO[Glue] = IO(new Glue(Box.createVerticalGlue()))

  def horizontal: IO[Glue] = IO(new Glue(Box.createHorizontalGlue()))

  implicit val GlueComponent = new Comp[Glue] {
    def peer(g: Glue) = g.peer
  }

  implicit val GlueElem: AsSingleElem[Glue] = Elem vhFill { _.peer }
}

// vim: set ts=2 sw=2 et:
