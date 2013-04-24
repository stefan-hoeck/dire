package dire.swing

import dire._
import java.awt.{BorderLayout, Color, Dimension}
import java.awt.event.{WindowListener, WindowEvent}
import javax.swing.JFrame
import scalaz._, Scalaz._, effect.IO

class Frame(val peer: JFrame)

object Frame {
//  def apply(): IO[Frame] = apply("")
//
//  def apply(title: String): IO[Frame] = IO(new Frame(new JFrame(title)))
}

// vim: set ts=2 sw=2 et:
