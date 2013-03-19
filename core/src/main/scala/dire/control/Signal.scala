package dire.control

import dire.{Event, Out, Time}
import scalaz._, Scalaz._, effect.IO

sealed trait RawSignal[+A] { self ⇒ 
}

// vim: set ts=2 sw=2 et:
