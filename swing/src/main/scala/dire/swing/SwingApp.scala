package dire.swing

import dire._
import scalaz._, Scalaz._, effect.{IO, SafeApp}

trait SwingApp {
  import Frame.FrameEvent

  protected def behavior(f: Frame): IO[(Elem, SIn[Any])]

  def run = for {
    frame   ← Frame()
    p       ← behavior(frame)
    (e, sf) = p
    _       ← frame addElem e
    _       ← frame.show
    _       ← EF.run(sf >>> frame.events){ Frame.Closing == _ }
    _       ← IO(System.exit(0))
  } yield ()
}

// vim: set ts=2 sw=2 et:
