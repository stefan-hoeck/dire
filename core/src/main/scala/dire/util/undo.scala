package dire.util

import dire._, SF.{id, connectOuts}
import scalaz._, Scalaz._
import scalaz.concurrent.Strategy

/** Provides utility signal functions for dealing with Undo/Redo
  * functionality.
  *
  * Typically, undo/redo events are looped back to the input of
  * the undo/redo stream to keep track of the actual state of the
  * editable data. This should not lead to the registering of a
  * new Undo/Redo handler, however. We therefor distinguish at 
  * the input level between changes coming from user input and
  * changes coming from undo/redo. This is done using `\/` where
  * by convention user input is wrapped in a right and undo/redo
  * in a left.
  */
object undo {

  /** A basic signal function that creates undoable events.
    *
    * This signal function does not make assumptions about where
    * events are coming from or what is going to happen with the
    * undoable data pair with one exception:
    * By convention, input wrapped in a Right is assumed to come
    * from user input while input wrapped in a Left comes from Undo/Redo
    * and leads therefor not to a new event of pairs being fired.
    *
    * In client code the output of this function is typically looped back
    * to its input at some point.
    */
  def basic[A]: SF[A \/ A, (A, A)] = {
    def undoablePair(p: (A \/ A, A \/ A)): Option[(A,A)] =
      p._2 map ((p._1 valueOr identity,_)) toOption

    id.slidingAsPairs collectO undoablePair
  }

  /** Signal function for registering undoable events at a central
    * Undo/Redo handler.
    *
    * See [[dire.swing.undo.sf]] for a possible use case with Swing
    * `UndoableEdit`s.
    */
  def withHandler[A](register: Out[A] ⇒ Out[(A,A)],
                     strategy: Option[Strategy]): SF[A \/ A,A] =
    basic[A] andThen connectOuts(register, strategy)
}

// vim: set ts=2 sw=2 et:
