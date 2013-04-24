package dire.swing

import dire._
import java.awt.{Font, Color, Component ⇒ AComponent, Container ⇒ AContainer}
import java.awt.event.{MouseMotionListener, MouseListener, KeyListener,
                       MouseEvent ⇒ JMouseEvent, KeyEvent ⇒ JKeyEvent,
                       FocusEvent ⇒ JFocusEvent, FocusListener} 
import javax.swing.JComponent
import javax.swing.border.Border

/** Type class representing a `java.awt.Component` */
trait Comp[-A] {
  import Component._

  def peer(a: A): AComponent

  final def background(a: A): Sink[Color] = sink(peer(a).setBackground)

  final def bounds(a: A): Sink[Rect] =
    sink(r ⇒ peer(a).setBounds(rectangle(r)))

  final def cursor(a: A): Sink[Cursor] = 
    sink(c ⇒ peer(a).setCursor(new java.awt.Cursor(c.v)))

  final def enabled(a: A): Sink[Boolean] = sink(peer(a).setEnabled)

  final def focus(a: A): SIn[FocusEvent] =
    SF.cachedSrc[AComponent,FocusEvent](peer(a))

  final def focusGained(a: A): SIn[Unit] =
    focus(a) collect { case FocusGained ⇒ () }

  final def focusLost(a: A): SIn[Unit] =
    focus(a) collect { case FocusLost ⇒ () }

  final def focusable(a: A): Sink[Boolean] = sink(peer(a).setFocusable)

  final def font(a: A): Sink[Font] = sink(peer(a).setFont)

  final def foreground(a: A): Sink[Color] = sink(peer(a).setForeground)

  final def keys(a: A): SIn[KeyEvent] =
    SF.cachedSrc[AComponent,KeyEvent](peer(a))

  final def leftClicks(a: A): SIn[Unit] =
    mouse(a) collect { case Clicked(Button1) ⇒ () }

  final def maxSize(a: A): Sink[Dim] = 
    sink(d ⇒ peer(a).setMaximumSize(dimension(d)))

  final def minSize(a: A): Sink[Dim] = 
    sink(d ⇒ peer(a).setMinimumSize(dimension(d)))

  final def mouse(a: A): SIn[MouseEvent] =
    SF.cachedSrc[AComponent,MouseEvent](peer(a))

  final def mouseMoved(a: A): SIn[MotionEvent] =
    SF.cachedSrc[AComponent,MotionEvent](peer(a))

  final def mousePosition(a: A): SIn[Position] = mouseMoved(a) map { _.pos }

  final def name(a: A): Sink[String] = sink(peer(a).setName)

  final def preferredSize(a: A): Sink[Dim] = 
    sink(d ⇒ peer(a).setPreferredSize(dimension(d)))

  final def rightClicks(a: A): SIn[Unit] =
    mouse(a) collect { case Clicked(Button3) ⇒ () }

  final def size(a: A): Sink[Dim] = 
    sink(d ⇒ peer(a).setSize(dimension(d)))

  final def visible(a: A): Sink[Boolean] = sink(peer(a).setVisible)
}

/** Type class representing a `java.awt.Container` */
trait Container[-A] extends Comp[A] {
  def peer(a: A): AContainer
}

/** Type class representing a `javax.swing.JComponent` */
trait Component[-A] extends Container[A] {
  def peer(a: A): JComponent

  final def border(a: A): Sink[Border] = sink(peer(a).setBorder)

  final def doubleBuffered(a: A): Sink[Boolean] =
    sink(peer(a).setDoubleBuffered)

  final def opaque(a: A): Sink[Boolean] = sink(peer(a).setOpaque)

  final def tooltip(a: A): Sink[Option[String]] = 
    sink(os ⇒ peer(a).setToolTipText(os getOrElse null))
}

object Comp {
  def apply[A:Comp]: Comp[A] = implicitly

  implicit val CompIso = new (Comp ~>> AComponent) {
    def apply[A](f: Comp[A], a: A) = f peer a
  }
}

object Container {
  def apply[A:Container]: Container[A] = implicitly

  implicit val ContainerIso = new (Container ~>> AContainer) {
    def apply[A](f: Container[A], a: A) = f peer a
  }
}

object Component {

  def apply[A:Component]: Component[A] = implicitly

  sealed trait MotionEvent { def pos: Position }

  case class Moved(pos: Position) extends MotionEvent
  case class Dragged(pos: Position) extends MotionEvent

  private def motionListener(o: MotionEvent ⇒ Unit) = new MouseMotionListener {
    def mouseDragged(x: JMouseEvent) { o(Dragged((x.getX, x.getY))) }
    def mouseMoved(x: JMouseEvent) { o(Moved((x.getX, x.getY))) }
  }

  sealed trait MouseEvent

  case class Pressed(button: MouseButton) extends MouseEvent
  case class Released(button: MouseButton) extends MouseEvent
  case class Clicked(button: MouseButton) extends MouseEvent
  case object Entered extends MouseEvent
  case object Exited extends MouseEvent

  sealed abstract class MouseButton(val v: Int)

  case object Button1 extends MouseButton(JMouseEvent.BUTTON1)
  case object Button2 extends MouseButton(JMouseEvent.BUTTON2)
  case object Button3 extends MouseButton(JMouseEvent.BUTTON3)
  case object NoButton extends MouseButton(JMouseEvent.NOBUTTON)

  private def button(e: JMouseEvent): MouseButton = e.getButton match {
    case JMouseEvent.BUTTON1 ⇒ Button1
    case JMouseEvent.BUTTON2 ⇒ Button2
    case JMouseEvent.BUTTON3 ⇒ Button3
    case _                   ⇒ NoButton
  }

  private def mouseListener(o: MouseEvent ⇒ Unit) = new MouseListener {
    def mouseClicked(e: JMouseEvent) { o(Clicked(button(e))) }
    def mousePressed(e: JMouseEvent) { o(Pressed(button(e))) }
    def mouseReleased(e: JMouseEvent) { o(Released(button(e))) }
    def mouseEntered(e: JMouseEvent) { o(Entered) }
    def mouseExited(e: JMouseEvent) { o(Exited) }
  }

  sealed trait KeyEvent {
    def mods: Int
    def loc: KeyLocation
  }

  case class KeyTyped(char: Char, mods: Int, loc: KeyLocation)
    extends KeyEvent {
    def this(e: JKeyEvent) = this(e.getKeyChar,
      e.getModifiersEx, KeyLocation get e.getKeyLocation)
  }

  case class KeyPressed(key: Key, mods: Int, loc: KeyLocation)
    extends KeyEvent {
    def this(e: JKeyEvent) = this(Key get e.getKeyCode,
      e.getModifiersEx, KeyLocation get e.getKeyLocation)
  }

  case class KeyReleased(key: Key, mods: Int, loc: KeyLocation)
    extends KeyEvent {
    def this(e: JKeyEvent) = this(Key get e.getKeyCode,
      e.getModifiersEx, KeyLocation get e.getKeyLocation)
  }

  private def keyListener(o: KeyEvent ⇒ Unit) = new KeyListener {
    def keyPressed(e: JKeyEvent) { o(new KeyPressed(e)) }
    def keyReleased(e: JKeyEvent) { o(new KeyReleased(e)) }
    def keyTyped(e: JKeyEvent) { o(new KeyTyped(e)) }
  }

  sealed trait FocusEvent

  case object FocusGained extends FocusEvent
  case object FocusLost extends FocusEvent

  private def focusListener(o: FocusEvent ⇒ Unit) = new FocusListener {
    def focusGained(e: JFocusEvent) { o(FocusGained) }
    def focusLost(e: JFocusEvent) { o(FocusLost) }
  }

  implicit val ComponentIso = new (Component ~>> JComponent) {
    def apply[A](f: Component[A], a: A) = f peer a
  }

  private[dire] implicit val MotionSource: Source[AComponent,MotionEvent] =
    eventSrc { c ⇒ o ⇒ 
      val l = motionListener(o)
      c.addMouseMotionListener(l)
      _ ⇒ c.removeMouseMotionListener(l)
    }

  private[dire] implicit val MouseSource: Source[AComponent,MouseEvent] =
    eventSrc { c ⇒ o ⇒ 
      val l = mouseListener(o)
      c.addMouseListener(l)
      _ ⇒ c.removeMouseListener(l)
    }

  private[dire] implicit val KeySource: Source[AComponent,KeyEvent] =
    eventSrc { c ⇒ o ⇒ 
      val l = keyListener(o)
      c.addKeyListener(l)
      _ ⇒ c.removeKeyListener(l)
    }

  private[dire] implicit val FocusSource: Source[AComponent,FocusEvent] =
    eventSrc { c ⇒ o ⇒ 
      val l = focusListener(o)
      c.addFocusListener(l)
      _ ⇒ c.removeFocusListener(l)
    }
}

sealed abstract class Cursor(val v: Int)

object Cursor {
  import java.awt.Cursor._

  case object Crosshair extends Cursor(CROSSHAIR_CURSOR)
  case object Custom extends Cursor(CUSTOM_CURSOR)
  case object Default extends Cursor(DEFAULT_CURSOR)
  case object EResize extends Cursor(E_RESIZE_CURSOR)
  case object Hand extends Cursor(HAND_CURSOR)
  case object Move extends Cursor(MOVE_CURSOR)
  case object NEResize extends Cursor(NE_RESIZE_CURSOR)
  case object NResize extends Cursor(N_RESIZE_CURSOR)
  case object NWResize extends Cursor(NW_RESIZE_CURSOR)
  case object SEResize extends Cursor(SE_RESIZE_CURSOR)
  case object SResize extends Cursor(S_RESIZE_CURSOR)
  case object SWResize extends Cursor(SW_RESIZE_CURSOR)
  case object Text extends Cursor(TEXT_CURSOR)
  case object WResize extends Cursor(W_RESIZE_CURSOR)
  case object Wait extends Cursor(WAIT_CURSOR)
}

// vim: set ts=2 sw=2 et:
