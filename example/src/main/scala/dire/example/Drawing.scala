package dire.example

import dire._
import dire.swing._, dire.swing.{Position ⇒ Pt}, Animation._
import scala.collection.immutable.{IndexedSeq ⇒ IxSeq}
import scalaz._, Scalaz._
import scalaz.std.indexedSeq._

object Drawing extends SwingApp {
  type Ps = IxSeq[Pt]
  val noPs: Ps = IxSeq.empty

  def behavior(f: Frame) = for {
    scene ← Scene()
    _     ← Elem(scene) prefDim (1000, 800) addToFrame f
  } yield draw(scene)

  def draw(s: Scene): SIn[Shape] = {
    def line(ps: Ps) = polyLine(ps, java.awt.Color.GREEN)

    val events = s.rightClicks or (s.mousePosition on s.leftClicks)
    val draw = events map lineSt scanStV noPs collectO identity
    val actual = (s.mousePosition ⊛ draw){ (p,d) ⇒ line((~d | noPs) :+ p) }
    val painting = draw collectO { _.toOption map line } sum
    
    (painting ⊛ actual){ _ ⊹ _ } to s.display
  }

  def lineSt(l: Unit \/ Pt) = State[Ps,Option[Ps \/ Ps]] { ps ⇒ 
    l.fold(_ ⇒ (noPs, if (ps.isEmpty) None else Some(ps.right)),
           p ⇒ { val n = ps :+ p; (n, Some(n.left)) })
  }
}

// vim: set ts=2 sw=2 et:
