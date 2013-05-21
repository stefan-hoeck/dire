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

  val tenIn = ten map Input
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

  def runUndo(es: List[Event]): List[Int] = simulate(es)(sf)

  sealed abstract class Event(val passed: Boolean)

  case class Input(i: Int) extends Event(false)
  case object Undo extends Event(true)
  case object Redo extends Event(true)

  def sf(o: Out[Unit]): IO[SF[Event,Int]] = IO {
    var us: List[UndoEdit] = Nil //undos
    var rs: List[UndoEdit] = Nil //redos

    def undo(u: UndoEdit) = u.un >> IO { rs = u :: rs; us = us.tail }
    def redo(u: UndoEdit) = u.re >> IO { us = u :: us; rs = rs.tail }

    val urOut: Out[UndoEdit] = u ⇒ IO { us = u :: us } >> o(())

    def onE(e: Event): IO[Unit] = e match {
      case Undo ⇒ us.headOption map undo orZero
      case Redo ⇒ rs.headOption map redo orZero
      case _    ⇒ IO.ioUnit
    }

    val undoSf = dire.swing.undo.sf[Int](urOut) map { _.left[Int] }

    (id syncTo onE collect { case Input(i) ⇒ i.right }) >=>
    loop(undoSf).collectL
  }
}

// vim: set ts=2 sw=2 et:
