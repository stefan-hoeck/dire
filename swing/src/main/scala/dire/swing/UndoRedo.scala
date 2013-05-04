package dire.swing

import dire._, SF.{id, connectOuts}
import scalaz._, Scalaz._, effect.IO

final case class UndoEdit(un: IO[Unit], re: IO[Unit])
  extends javax.swing.undo.AbstractUndoableEdit {

  override def undo() {super.undo(); un.unsafePerformIO}

  override def redo() {super.redo(); re.unsafePerformIO}

  def event(src: Any) =
    new javax.swing.event.UndoableEditEvent(src, this)
}

object UndoEdit {
  //Convention: Rights come from new input, Lefts from Undo, Redo
  //First is old value, second is new value
  type UEDis[+A] = (A,A) \/ (A,A) 

  //Accumulates Undo/Redo pairs. As soon as an event stream fired
  //two events, we have a pair of an old and a new value.
  //The latest event might have come from user input in which
  //case it is wrapped in a Right
  //and the resulting pair is also wrapped in a right. If
  //the lates event comes from Undo/Redo, the result is
  //wrapped in a left. So, every change is accumulated but only
  //pairs wrapped in a Right will be passed on to the Undo/Redo
  //machinery, pairs wrapped in a Left will be ignored by Undo/Redo.
  private[swing] def foldPair[A](p: (A \/ A, A \/ A)): UEDis[A] = {
    val o = p._1 valueOr identity
    
    p._2 bimap ((o,_), (o,_))
  }

  /** Returns a signal function that takes items of type `A \/ A` as input
    * and fires events of type `A` whenever an Undo or Redo event
    * happens.
    *
    * By convention, input events wrapped in a Right come from user input
    * while those wrapped in a Left come from Undo/Redo
    */
  def sf[A](out: Out[UndoEdit]): SF[A \/ A,A] = {
    //takes an Out[A] (provided by an internal Var by dire) and returns an
    //Out[UEDis[A]]. Used to create a signal function via SF.connectOuts
    def toEdit(oa: Out[A]): Out[UEDis[A]] = _ match {
      case \/-((o,n)) ⇒ out(UndoEdit(oa(o), oa(n)))
      case _          ⇒ IO.ioUnit
    }

    def toEditSF: SF[UEDis[A],A] = connectOuts(toEdit, SwingStrategy)

    id[A\/A].slidingAsPairs map foldPair[A] andThen toEditSF
  }
}

// vim: set ts=2 sw=2 et:
