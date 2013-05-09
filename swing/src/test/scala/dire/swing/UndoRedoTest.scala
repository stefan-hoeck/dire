package dire.swing

import dire.SF
import dire.DataSink.buffer
import dire.control.{Var, ReactiveSystem}
import org.scalacheck._, Prop._
import scalaz._, Scalaz._, effect.{IO, IORef}
import scalaz.concurrent.Strategy.Sequential

object UndoRedoTest extends org.scalacheck.Properties("Undo/Redo") {
  val ten = 1 to 10 toList

  property("undo") = {
    val events: List[UREvent] = ten.map(Input) ::: ten.as(Undo)
    val exp = (1 to 9).toList.reverse

    UndoMock(events, exp).unsafePerformIO()
  }

  property("redo") = {
    val events: List[UREvent] = ten.map(Input) ::: ten.as(Undo) ::: ten.as(Redo)
    val exp = (1 to 9).toList.reverse ::: (2 to 10).toList

    UndoMock(events, exp).unsafePerformIO()
  }
}

sealed trait UREvent

case class Input(i: Int) extends UREvent
case object Undo extends UREvent
case object Redo extends UREvent

class UndoMock private(es: List[UREvent]) {
  private var undos: List[UndoEdit] = Nil
  private var redos: List[UndoEdit] = Nil
  private val ints = new collection.mutable.ListBuffer[Int]
  private val events = new collection.mutable.ListBuffer[Int \/ Int]

  private def urOut(ur: UndoEdit): IO[Unit] = IO { undos = ur :: undos }

  private def doUndo: IO[Unit] = undos.headOption.cata(
    h ⇒ h.un >> IO { undos = undos.tail; redos = h :: redos },
    IO.ioUnit
  )

  private def doRedo: IO[Unit] = redos.headOption.cata(
    h ⇒ h.re >> IO { redos = redos.tail; undos = h :: undos },
    IO.ioUnit
  )

  def sf = {
    val nil = List.empty[Int]

    val id = SF.id[UREvent]

    val is = id.collect { case Input(i) ⇒ i.right[Int] }
                .to(buffer(events))
                .andThen(dire.swing.undo.sf[Int](urOut, Some(Sequential)))
                .to(buffer(ints))
                .scanPlus[List]

    val undo = id collect { case Undo ⇒ nil } syncTo { _ ⇒ doUndo }
    val redo = id collect { case Redo ⇒ nil } syncTo { _ ⇒ doRedo }

    SF all es andThen (is ⊹ undo ⊹ redo)
  }
  
  def report = IO {
    println("Ints: ")
    println(ints.toList mkString "\n")
    println("")
    println("Events: ")
    println(events.toList mkString "\n")
  }
}

object UndoMock {
  def apply(es: List[UREvent], exp: List[Int]): IO[Boolean] = for {
    m   ← IO(new UndoMock(es))
    _   ← SF.run(m.sf, 2){ exp ≟ _ }
  } yield true
}

// vim: set ts=2 sw=2 et:
