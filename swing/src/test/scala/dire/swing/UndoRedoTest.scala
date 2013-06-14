package dire.swing

import dire.{SF, Out}, SF.{id, loop}
import dire.DataSink.buffer
import dire.control.{Var, ReactiveSystem}
import org.scalacheck._, Prop._
import scalaz._, Scalaz._, effect.{IO, IORef}
import scalaz.concurrent.Strategy.Sequential

object UndoRedoTest
  extends org.scalacheck.Properties("Undo/Redo")
  with dire.util.TestFunctions {
  val ten = 1 to 10 toList

  val tenIn = ten map In
  val tenUndo = ten.tail as Undo
  val tenRedo = ten.tail as Redo

  property("undo") = {
    val events: List[Event] = tenIn ::: tenUndo
    val exp = (1 to 9).toList.reverse

    runUndo(events) ≟ exp
  }

  property("redo") = {
    val events: List[Event] = tenIn ::: tenUndo ::: tenRedo
    val exp = (1 to 9).reverse ++ (2 to 10) toList

    runUndo(events) ≟ exp
  }

  def runUndo(es: List[Event]): List[Int] = simulate(es, false)(sf)

  sealed abstract class Event(val passed: Boolean)

  case class In(i: Int) extends Event(false)
  case object Undo extends Event(true)
  case object Redo extends Event(true)

  //When to confirm event processing is finished?
  //Undoable events are to be confirmed after undo is registered (urOut)
  //Non undoable events are to be confirmed when they happen
  def sf(o: Out[Any]): IO[SF[Event,Int]] = IO {
    var us: List[UndoEdit] = Nil //undos
    var rs: List[UndoEdit] = Nil //redos

    def undo(u: UndoEdit) = IO { rs = u :: rs; us = us.tail } >> u.un
    def redo(u: UndoEdit) = IO { us = u :: us; rs = rs.tail } >> u.re

    val urOut: Out[UndoEdit] = u ⇒ IO { us = u :: us } >> o(0)

    def onE(e: Event): IO[Unit] = e match {
      case Undo ⇒ us.headOption map undo orZero
      case Redo ⇒ rs.headOption map redo orZero
      case _    ⇒ IO.ioUnit
    }

    val undoSf = 
      dire.swing.undo.sf[Int](urOut) map { _.left[Int] } syncTo o

    //Hold is important here otherwise the first Input event will neither
    //make it to urOut nor to the end of undoSf and therefore will never
    //be confirmed
    (id[Event] hold In(-1) syncTo onE collect { case In(i) ⇒ i.right }) >=>
    loop(undoSf).collectL
  }
}

// vim: set ts=2 sw=2 et:
