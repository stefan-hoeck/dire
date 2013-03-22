package dire.control

import dire.{Time, T0}
import org.scalacheck._, Prop._
import scalaz._, Scalaz._, effect.{IO, IORef}

object NodeTest extends Properties("Node") {

  class MockChild(val calcRes: Boolean, val id: Int) extends ChildNode {
    var updateTime = T0
    var cleaned = 0
    var calced = 0
    def doCalc(t: Time) = { calced += 1; updateTime = t; calcRes }
    def doClean() { cleaned += 1 }
  }

  type Nodes = (RootNode, MockChild, MockChild, MockChild, MockChild)

  //A simple dependency graph
  def runGraph(res1: Boolean, res2: Boolean, res3: Boolean, res4: Boolean): Nodes = {
    val r = new RootNode
    val c1 = new MockChild(res1, 1)
    val c2 = new MockChild(res2, 2)
    val c3 = new MockChild(res3, 3)
    val c4 = new MockChild(res4, 4)

    r connectChild c1
    r connectChild c2
    c1 connectChild c3
    c2 connectChild c3
    c3 connectChild c4

    r.update(1L)

    (r, c1, c2, c3, c4)
  }

  val bools = List(true, false)

  val allRuns: List[Nodes] = ^^^(bools, bools, bools, bools)(runGraph)

  val allChildren: List[MockChild] =
    allRuns >>= { case (r,a,b,c,d) ⇒ List(a,b,c,d)}

  property("updated at most once") = allChildren ∀ { _.calced <= 1L }

  property("cleaned at most once") = allChildren ∀ { _.calced <= 1L }

  property("called with proper time") = allChildren ∀ { c ⇒ 
    if (c.calced ≟ 0) c.updateTime ≟ T0
    else c.updateTime ≟ 1L
  }

  property("cleaned iff updated and changed") = allChildren ∀ { c ⇒ 
    ((c.calcRes) && (c.calced ≟ 1)) ≟ (c.cleaned ≟ 1)
  }

  property("updated iff at least one parent updated") = allChildren ∀ { c ⇒ 
    ((c.calced ≟ 1) ≟ (c.getParents ∃ hasChanged))
  }

  def hasChanged(n: Node) = n match {
    case m: MockChild ⇒ (m.calced >= 1) && m.calcRes
    case r: RootNode  ⇒ r.hasChanged
    case _            ⇒ sys.error("What!?")
  }

  def printError(c: MockChild) = {
    def single(c: MockChild) = s"(${c.id}; calced: ${c.calced ≟ 1}; res: ${c.calcRes}) "

    def singleN(n: Node) = n match {
      case m: MockChild ⇒ single(m)
      case r: RootNode  ⇒ "(Root) "
      case _            ⇒ sys.error("What!?")
    }

    def parents = c.getParents foldMap singleN

    println(s"${single(c)}; $parents")
  }
}

// vim: set ts=2 sw=2 et:

