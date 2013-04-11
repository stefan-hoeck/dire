package dire.control

import dire._
import org.scalacheck._, Prop._
import scalaz._, Scalaz._, effect.IO

//object ReactorTest extends Properties("Reactor") with Runner{
//
//  property("time_run") = forAll(Gen choose (1, 10)) { i ⇒ 
//    val id = Arrow[SF].id[Time]
//
//    val coll = runFor(id, i)
//    val exp  = 0 to i map { x ⇒ Change(x, x: Long) } toList
//
//    coll ≟ exp :| s"Exp: $exp, found $coll"
//  }
//}

// vim: set ts=2 sw=2 et:
