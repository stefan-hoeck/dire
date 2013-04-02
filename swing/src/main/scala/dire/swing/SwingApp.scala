package dire.swing

import dire._
import scalaz._, Scalaz._, effect.{IO, SafeApp}

trait SwingApp {
  import Frame.FrameEvent

  protected def behavior: IO[(Elem, SF[Event[FrameEvent],FrameV])]

  def run = for {
    p       ← behavior
    (e, sf) = p
    frame   ← Frame()
    _       ← frame addElem e
    _       ← frame.show
    _       ← EF.run(EF loop (sf >>> frame.sf)){ Frame.Closing == _ }
    _       ← IO(System.exit(0))
  } yield ()
}

// vim: set ts=2 sw=2 et:
