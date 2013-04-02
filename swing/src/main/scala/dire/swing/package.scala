package dire

import java.awt.event.{ActionListener, ActionEvent}
import scalaz._, Scalaz._, effect.IO
import scalaz.concurrent.Strategy.SwingInvokeLater

package object swing {

  type Sink[S,A] = DataSink[S,A]
  type Source[S,A] = DataSource[S,A]
  type ESource[S,A] = Source[S,Event[A]]
  type Callback[-S,+A] = S ⇒ (A ⇒ Unit) ⇒ (Unit ⇒ Unit)

  type Dim = (Int, Int)

  private[swing] def sink[S,A](out: S ⇒ Out[A]): Sink[S,A] =
    DataSink.create[S,A](out, _ ⇒ IO.ioUnit, Some(SwingInvokeLater))

  private[swing] def eventSrc[S,A](out: Callback[S,A]): ESource[S,A] =
    DataSource eventSrcInpure out

  private[swing] def ali(out: Unit ⇒ Unit): ActionListener = 
    new ActionListener { def actionPerformed(e: ActionEvent) = out(()) }
}

// vim: set ts=2 sw=2 et:
