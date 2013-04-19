package dire.swing

import dire._
import java.awt.{Color, Font}
import javax.swing.{JTextField}
import javax.swing.border.Border
import scalaz._, Scalaz._, effect.IO

case class TextField(peer: JTextField) extends TextComponent[JTextField] {
  import TextField._

  def actions: SIn[Unit] = SF cachedSrc this

  def textEvents: SIn[String] = value on actions
}

object TextField {
  private[this] lazy val dummy = new JTextField

  def apply(text: String = "",
            background: Color = dummy.getBackground,
            border: Border = dummy.getBorder,
            editable: Boolean = true,
            font: Font = dummy.getFont,
            foreground: Color = dummy.getForeground,
            horizontalAlignment: Alignment = Alignment.Leading,
            opaque: Boolean = dummy.isOpaque,
            tooltip: Option[String] = None): IO[TextField] = IO {
    val t = new JTextField(text)
    t.setBackground(background)
    t.setBorder(border)
    t.setEditable(editable)
    t.setFont(font)
    t.setForeground(foreground)
    t.setHorizontalAlignment(horizontalAlignment.v)
    t.setOpaque(opaque)
    t.setToolTipText(tooltip getOrElse null)

    TextField(t)
  }

  def trailing(text: String = ""): IO[TextField] =
    apply(text, horizontalAlignment = Alignment.Trailing)

  implicit val TextFieldElem: AsSingleElem[TextField] = Elem hFill { _.peer }

  implicit val TextFieldSource: Source[TextField,Unit] = eventSrc { t ⇒ o ⇒ 
    val a = ali(o)
    t.peer.addActionListener(a)
    _ ⇒ t.peer.removeActionListener(a)
  }
}

// vim: set ts=2 sw=2 et:
