package dire.swing

import dire._, SF.{id, connectOuts}
import dire.util.undo.withHandler
import scalaz._, Scalaz._, effect.IO
import scalaz.concurrent.Strategy

final case class UndoEdit(un: IO[Unit], re: IO[Unit])
  extends javax.swing.undo.AbstractUndoableEdit {

  override def undo() {super.undo(); un.unsafePerformIO}

  override def redo() {super.redo(); re.unsafePerformIO}

  def event(src: Any) =
    new javax.swing.event.UndoableEditEvent(src, this)
}

object undo {
  /** Returns a signal function that takes items of type `A \/ A` as input
    * and fires events of type `A` whenever an Undo or Redo event
    * happens.
    *
    * By convention, input events wrapped in a Right come from user input
    * while those wrapped in a Left come from Undo/Redo
    */
  def sf[A](out: Out[UndoEdit],
            strategy: Option[Strategy] = SwingStrategy): SF[A \/ A,A] =
    withHandler[A](oa ⇒ p ⇒ out(UndoEdit(oa(p._1), oa(p._2))), strategy)
}

// vim: set ts=2 sw=2 et:
