package dire.swing

import java.awt.{GridBagLayout, Dimension}
import javax.swing.JPanel
import scalaz._, Scalaz._, effect.IO

final class Panel(val peer: JPanel)

object Panel {
  def apply(props: Panel ⇒ IO[Unit]*): IO[Panel] = for {
    p   ← IO(new JPanel)
    _   ← IO(p.setLayout(new GridBagLayout))
    res ← IO(new Panel(p))
    _   ← props.toList foldMap { _(res) }
  } yield res

  implicit val PanelComponent: Component[Panel] = new Component[Panel] {
    def peer(p: Panel) = p.peer
  }

  implicit val PanelElem: AsElem[Panel] = Elem vhFill { _.peer }
}

// vim: set ts=2 sw=2 et:
