package dire.swing

import java.awt.{GridBagLayout, Dimension}
import javax.swing.JPanel
import scalaz.effect.IO

class Panel private(val peer: JPanel) extends Component[JPanel]

object Panel {
  def apply(): IO[Panel] = IO(new JPanel) flatMap apply

  def apply(p: JPanel): IO[Panel] = for {
    _ ← IO(p.setLayout(new GridBagLayout))
    r ← IO(new Panel(p))
  } yield r

  implicit val PanelElem: AsElem[Panel] = Elem vhFill { _.peer }
}

// vim: set ts=2 sw=2 et:
