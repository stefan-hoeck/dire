package dire.swing

import dire.control.{Var, ReactiveSystem}
import org.scalacheck._, Prop._
import scalaz._, Scalaz._, effect.{IO, IORef}

object UndoRedoTest extends org.scalacheck.Properties("Undo/Redo") {
  property("foldPair") = forAll { p: (Either[Int,Int], Either[Int,Int]) ⇒
    val o = \/ fromEither p._1
    val n = \/ fromEither p._2
    val ov = o valueOr identity
    val nv = n valueOr identity

    val res = UndoEdit foldPair (o, n)

    (res.isLeft ≟ n.isLeft) &&
    ((ov, nv) ≟ res.valueOr(identity))
  }

  property("undo") = {
    val run = for {
      mock ← UndoMock()
      _    ← (1 to 10).toList foldMap mock.v.put
      _    ← IO(Thread.sleep(10))
      _    ← (1 to 10).toList foldMap { _ ⇒ mock.undo }
      _    ← IO(Thread.sleep(10))
      _    ← mock.shutdown
      _    ← mock.report
    } yield mock.getInts

    val res = run.unsafePerformIO()

    res ≟ (0 to 9).toList.reverse
  }

  property("redo") = {
    val run = for {
      mock ← UndoMock()
      _    ← (1 to 10).toList foldMap mock.v.put
      _    ← IO(Thread.sleep(10))
      _    ← (1 to 10).toList foldMap { _ ⇒ mock.undo }
      _    ← IO(Thread.sleep(10))
      _    ← (1 to 10).toList foldMap { _ ⇒ mock.redo }
      _    ← IO(Thread.sleep(10))
      _    ← mock.shutdown
      _    ← mock.report
    } yield mock.getInts

    val res = run.unsafePerformIO()

    res ≟ ((0 to 9).toList.reverse ::: (1 to 10).toList)
  }
}

class UndoMock private(
    rs: ReactiveSystem,
    val v: Var[Int],
    val undoV: Var[Option[Unit]],
    val redoV: Var[Option[Unit]]) {
  private var undos: List[UndoEdit] = Nil
  private var redos: List[UndoEdit] = Nil
  private val ints = new collection.mutable.ListBuffer[Int]
  private val events = new collection.mutable.ListBuffer[Int \/ Int]

  private def urOut(ur: UndoEdit): IO[Unit] = IO { undos = ur :: undos }

  private def intOut(i: Int): IO[Unit] = IO { ints += i }

  private def eventOut(i: Int \/ Int): IO[Unit] = IO { events += i }

  def undo = undoV put ().some

  def redo = redoV put ().some

  private def doUndo: IO[Unit] = undos.headOption.cata(
    h ⇒ h.un >> IO { undos = undos.tail; redos = h :: redos },
    IO.ioUnit
  )

  private def doRedo: IO[Unit] = redos.headOption.cata(
    h ⇒ h.re >> IO { redos = redos.tail; undos = h :: undos },
    IO.ioUnit
  )

  def getInts = ints.toList

  def shutdown = rs.shutdown

  def sf = {
    val ints = v.in.map { _.right[Int] }
                .syncTo(eventOut)
                .andThen(UndoEdit.sf[Int](urOut))
                .syncTo(intOut)

    val undo = undoV.in collectO identity syncTo { _ ⇒ doUndo }
    val redo = redoV.in collectO identity syncTo { _ ⇒ doRedo }

    ints >> undo >> redo
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
  def apply(): IO[UndoMock] = for {
    rs  ← ReactiveSystem()
    v   ← Var newVar 0
    vu  ← Var newVar none[Unit]
    vr  ← Var newVar none[Unit]
    m   ← IO(new UndoMock(rs, v, vu, vr))
    _   ← rs forever m.sf
    _   ← IO(Thread.sleep(100))
  } yield m
}

// vim: set ts=2 sw=2 et:
