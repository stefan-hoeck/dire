package dire.swing

import dire._
import scalaz._, Scalaz._, effect.{IO, SafeApp}

trait SwingApp extends SafeApp {
  import Window.WindowEvent, Swing._

  protected def behavior(f: Frame): IO[SIn[_]]

  // scalaz.effect.SafeApp entry point
  override def runc = for {
    frame   ← Frame()
    sf      ← behavior(frame)
    _       ← frame properties (visible := true)
    _       ← SF.run(sf >> frame.windowEvents){ Window.Closing == _ }
    _       ← IO(System.exit(0))
  } yield ()
}

// vim: set ts=2 sw=2 et:
