package dire.swing

import dire._
import javax.swing.{JLabel, Icon}
import scalaz._, Scalaz._, effect.IO

final class Label(val peer: JLabel)

object Label {
  def apply(props: Label ⇒ IO[Unit]*): IO[Label] = for {
    res ← IO(new Label(new JLabel()))
    _   ← props.toList foldMap { _(res) }
  } yield res

  implicit val LabelComponent =
    new Component[Label] 
    with TextDisplay[Label]
    with IconDisplay[Label] {
      def peer(l: Label) = l.peer

      def setHAlign(a: Label, h: HAlign) =
        IO(peer(a).setHorizontalAlignment(h.v))

      def setHTextPos(a: Label, h: HAlign) =
        IO(peer(a).setHorizontalTextPosition(h.v))

      def setText(a: Label, s: String) = IO(peer(a).setText(s))

      def setVAlign(a: Label, v: VAlign) =
        IO(peer(a).setVerticalAlignment(v.v))

      def setVTextPos(a: Label, v: VAlign) =
        IO(peer(a).setVerticalTextPosition(v.v))

      def setDisabledIcon(a: Label, i: Icon) = IO(peer(a).setDisabledIcon(i))
      
      def setIcon(a: Label, i: Icon) = IO(peer(a).setIcon(i))

      def setIconTextGap(a: Label, i: Int) = IO(peer(a).setIconTextGap(i))
    }

  implicit val LabelElem: AsSingleElem[Label] = Elem noFill { _.peer }
}

// vim: set ts=2 sw=2 et:
