package dire.swing

import java.awt.{GridBagLayout, Dimension}
import javax.swing.JPanel
import scalaz._, Scalaz._, effect.IO

final class Panel(val peer: JPanel)

object Panel {
  def apply(props: (Panel ⇒ IO[Unit])*): IO[Panel] = for {
    res ← IO(new Panel(new JPanel()))
    _   ← props.toList foldMap { _(res) }
  } yield res

  implicit val PanelComponent: Component[Panel] = new Component[Panel] {
    def peer(p: Panel) = p.peer
  }

  implicit val PanelElem: AsSingleElem[Panel] = Elem vhFill { _.peer }
}

// vim: set ts=2 sw=2 et:
