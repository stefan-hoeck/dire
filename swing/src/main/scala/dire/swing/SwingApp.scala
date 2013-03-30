package dire.swing

import dire._
import scalaz._, Scalaz._, effect.{IO, SafeApp}

trait SwingApp extends SafeApp {
  import Frame.FrameEvent

  protected def behavior: IO[(Elem, SF[Event[FrameEvent],FrameV])]

  override def runc = for {
    p       ← behavior
    (e, sf) = p
    frame   ← Frame()
    _       ← frame addElem e
    _       ← frame.show
    _       ← SF.runE(SF loopE (sf andThen frame.sf)){ e ⇒ 
                println(e)
                Frame.Closing == e
              }
    _       ← IO(System.exit(0))
  } yield ()
}

// vim: set ts=2 sw=2 et:
