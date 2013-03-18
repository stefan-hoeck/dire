package dire.control

import org.scalacheck._, Prop._
import scalaz._, Scalaz._, effect.{IO, IORef}

object RNodeTest extends Properties("Node") {

  def run(p: IO[Prop]): Prop = p.unsafePerformIO

//  property("updates self") = run(
//    for {
//      called  ← IO newIORef false
//      n       ← RNode(called write true as true, IO.ioUnit)
//      _       ← n.update
//      updated ← called.read
//    } yield updated :| "Updated self"
//  )
//
//  property("updates child if calc returns true") = run(
//    for {
//      called  ← IO newIORef false
//      p       ← RNode(IO(true), IO.ioUnit)
//      c       ← RNode(called write true as true, IO.ioUnit)
//      _       ← p.connectChild(c).run
//      _       ← p.update
//      updated ← called.read
//    } yield updated :| "Updated child"
//  )
//
//  property("does not update child if calc returns false") = run(
//    for {
//      called  ← IO newIORef false
//      p       ← RNode(IO(false), IO.ioUnit)
//      c       ← RNode(called write true as true, IO.ioUnit)
//      _       ← p.connectChild(c).run
//      _       ← p.update
//      updated ← called.read
//    } yield !updated :| "child not updated"
//  )
//
//  property("does not update child if disconnected") = run(
//    for {
//      called  ← IO newIORef false
//      p       ← RNode(IO(true), IO.ioUnit)
//      c       ← RNode(called write true as true, IO.ioUnit)
//      x       ← p.connectChild(c).run
//      _       ← Connector disconnect x._1
//      _       ← p.update
//      updated ← called.read
//    } yield !updated :| "child not updated"
//  )
//
//  property("cleans self if calc returns true") = run(
//    for {
//      called  ← IO newIORef false
//      n       ← RNode(IO(true), called write true)
//      _       ← n.update
//      cleaned ← called.read
//    } yield cleaned :| "cleaned self"
//  )
//
//  property("does not clean self if calc returns false") = run(
//    for {
//      called  ← IO newIORef false
//      n       ← RNode(IO(false), called write true)
//      _       ← n.update
//      cleaned ← called.read
//    } yield !cleaned :| "not cleaned self"
//  )
//
//  property("cleans child if both return true") = run(
//    for {
//      called  ← IO newIORef false
//      p       ← RNode(IO(true), IO.ioUnit)
//      c       ← RNode(IO(true), called write true )
//      _       ← p.connectChild(c).run
//      _       ← p.update
//      cleaned ← called.read
//    } yield cleaned :| "cleaned child"
//  )
//
//  property("does not clean child if child returns false") = run(
//    for {
//      called  ← IO newIORef false
//      p       ← RNode(IO(true), IO.ioUnit)
//      c       ← RNode(IO(false), called write true )
//      _       ← p.connectChild(c).run
//      _       ← p.update
//      cleaned ← called.read
//    } yield !cleaned :| "not cleaned child"
//  )
//
//  property("does not clean child if parent returns false") = run(
//    for {
//      called  ← IO newIORef false
//      p       ← RNode(IO(false), IO.ioUnit)
//      c       ← RNode(IO(true), called write true )
//      _       ← p.connectChild(c).run
//      _       ← p.update
//      cleaned ← called.read
//    } yield !cleaned :| "not cleaned child"
//  )
//
//  property("updates self only once even if cyclic") = run(
//    for {
//      called  ← IO newIORef 0
//      n       ← RNode(called mod { 1 + _ } as true, IO.ioUnit)
//      _       ← n.connectChild(n).run
//      _       ← n.update
//      updated ← called.read
//    } yield (updated ≟ 1) :| s"Updated self only once: $updated"
//  )
}

// vim: set ts=2 sw=2 et:

