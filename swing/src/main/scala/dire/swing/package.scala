package dire

import java.awt.event.{ActionListener, ActionEvent}
import scalaz._, Scalaz._, effect.IO

package object swing {

  type Sink[-A] = DataSink[A]
  type Source[S,A] = DataSource[S,A]
  type Callback[-S,+A] = S ⇒ (A ⇒ Unit) ⇒ (Unit ⇒ Unit)

  type Dim = (Int, Int)

  type Rect = (Int, Int, Int, Int)

  type Position = (Int, Int)

  def swingOut[A](out: Out[A]): Out[A] = a ⇒ IO {
    javax.swing.SwingUtilities.invokeLater(
      new Runnable { def run { out(a).unsafePerformIO() } }
    )
  }

  def swingSink[A](out: Out[A]): Sink[A] = DataSink.sync[A](swingOut(out))

  private[swing] def dimension(d: Dim) =
    new java.awt.Dimension(d._1, d._2)

  private[swing] def rectangle(r: Rect) =
    new java.awt.Rectangle(r._1, r._2, r._3, r._4)

  private[swing] def sink[A](out: A ⇒ Unit): Sink[A] =
    sinkIO[A](a ⇒ IO(out(a)))

  private[swing] def sinkIO[A](out: A ⇒ IO[Unit]): Sink[A] = swingSink(out)

  private[swing] def eventSrc[S,A](out: Callback[S,A]): Source[S,A] =
    DataSource eventSrcImpure out

  private[swing] def src[S,A](ini: S ⇒ A)(out: Callback[S,A]): Source[S,A] =
    DataSource.signalSrcImpure(ini)(out)

  private[swing] def ali(out: Unit ⇒ Unit): ActionListener = 
    new ActionListener { def actionPerformed(e: ActionEvent) = out(()) }
}

// vim: set ts=2 sw=2 et:
