package dire

import java.awt.event.{ActionListener, ActionEvent}
import scala.reflect.runtime.universe.TypeTag
import scalaz._, Scalaz._, effect.IO
import scalaz.concurrent.Strategy.SwingInvokeLater

package object swing {

  type Sink[-A] = DataSink[A]
  type Source[S,A] = DataSource[S,A]
  type Callback[-S,+A] = S ⇒ (A ⇒ Unit) ⇒ (Unit ⇒ Unit)

  type Dim = (Int, Int)

  type Position = (Int, Int)

  private[swing] def sink[A:TypeTag](out: A ⇒ Unit, key: Any): Sink[A] =
    DataSink.cached[A](a ⇒ IO(out(a)), key, strategy = Some(SwingInvokeLater))

  private[swing] def blockedSink[S<:BlockedSignal,A:TypeTag]
    (s: S)(out: A ⇒ Unit): Sink[A] = 
    sink(a ⇒ { s.blocked = true; out(a); s.blocked = false }, s)

  private[swing] def eventSrc[S,A](out: Callback[S,A]): Source[S,A] =
    DataSource eventSrcInpure out

  private[swing] def src[S,A](ini: S ⇒ A)(out: Callback[S,A]): Source[S,A] =
    DataSource.signalSrcInpure(ini)(out)

  private[swing] def ali(out: Unit ⇒ Unit): ActionListener = 
    new ActionListener { def actionPerformed(e: ActionEvent) = out(()) }
}

// vim: set ts=2 sw=2 et:
