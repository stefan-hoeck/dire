package dire.swing

import java.awt.event.KeyEvent._
import scalaz.Equal

sealed abstract class Key(val v: Int) {
  def text = getKeyText(v)
}

sealed abstract class KeyLocation(val v: Int) {
  def text = getKeyText(v)
}

object KeyLocation {
  case object Left extends KeyLocation(KEY_LOCATION_LEFT)
  case object Right extends KeyLocation(KEY_LOCATION_RIGHT)
  case object Numpad extends KeyLocation(KEY_LOCATION_NUMPAD)
  case object Standard extends KeyLocation(KEY_LOCATION_STANDARD)
  case object Unknown extends KeyLocation(KEY_LOCATION_UNKNOWN)

  val values: List[KeyLocation] =
    List(Left, Right, Numpad, Standard, Unknown)

  val get: Map[Int,KeyLocation] = values map { k ⇒ k.v → k } toMap
  
  implicit val KeyLocationEqual = Equal.equalA[KeyLocation]
}

object Key {
  case object Shift extends Key(VK_SHIFT)
  case object Control extends Key(VK_CONTROL)
  case object Alt extends Key(VK_ALT)
  case object AltGraph extends Key(VK_ALT_GRAPH)
  case object Meta extends Key(VK_META)

  case object Enter extends Key(VK_ENTER)
  case object BackSpace extends Key(VK_BACK_SPACE)
  case object Tab extends Key(VK_TAB)
  case object Cancel extends Key(VK_CANCEL)
  case object Clear extends Key(VK_CLEAR)

  case object Pause extends Key(VK_PAUSE)
  case object CapsLock extends Key(VK_CAPS_LOCK)
  case object Escape extends Key(VK_ESCAPE)
  case object Space extends Key(VK_SPACE)
  case object PageUp extends Key(VK_PAGE_UP)
  case object PageDown extends Key(VK_PAGE_DOWN)
  case object End extends Key(VK_END)
  case object Home extends Key(VK_HOME)
  case object Left extends Key(VK_LEFT)
  case object Up extends Key(VK_UP)
  case object Right extends Key(VK_RIGHT)
  case object Down extends Key(VK_DOWN)
  case object Comma extends Key(VK_COMMA)
  case object Minus extends Key(VK_MINUS)
  case object Period extends Key(VK_PERIOD)
  case object Slash extends Key(VK_SLASH)
  case object Key0 extends Key(VK_0)
  case object Key1 extends Key(VK_1)
  case object Key2 extends Key(VK_2)
  case object Key3 extends Key(VK_3)
  case object Key4 extends Key(VK_4)
  case object Key5 extends Key(VK_5)
  case object Key6 extends Key(VK_6)
  case object Key7 extends Key(VK_7)
  case object Key8 extends Key(VK_8)
  case object Key9 extends Key(VK_9)
  case object Semicolon extends Key(VK_SEMICOLON)
  case object Equals extends Key(VK_EQUALS)
  case object A extends Key(VK_A)
  case object B extends Key(VK_B)
  case object C extends Key(VK_C)
  case object D extends Key(VK_D)
  case object E extends Key(VK_E)
  case object F extends Key(VK_F)
  case object G extends Key(VK_G)
  case object H extends Key(VK_H)
  case object I extends Key(VK_I)
  case object J extends Key(VK_J)
  case object K extends Key(VK_K)
  case object L extends Key(VK_L)
  case object M extends Key(VK_M)
  case object N extends Key(VK_N)
  case object O extends Key(VK_O)
  case object P extends Key(VK_P)
  case object Q extends Key(VK_Q)
  case object R extends Key(VK_R)
  case object S extends Key(VK_S)
  case object T extends Key(VK_T)
  case object U extends Key(VK_U)
  case object V extends Key(VK_V)
  case object W extends Key(VK_W)
  case object X extends Key(VK_X)
  case object Y extends Key(VK_Y)
  case object Z extends Key(VK_Z)
  case object OpenBracket extends Key(VK_OPEN_BRACKET)
  case object BackSlash extends Key(VK_BACK_SLASH)
  case object CloseBracket extends Key(VK_CLOSE_BRACKET)
  case object Numpad0 extends Key(VK_NUMPAD0)
  case object Numpad1 extends Key(VK_NUMPAD1)
  case object Numpad2 extends Key(VK_NUMPAD2)
  case object Numpad3 extends Key(VK_NUMPAD3)
  case object Numpad4 extends Key(VK_NUMPAD4)
  case object Numpad5 extends Key(VK_NUMPAD5)
  case object Numpad6 extends Key(VK_NUMPAD6)
  case object Numpad7 extends Key(VK_NUMPAD7)
  case object Numpad8 extends Key(VK_NUMPAD8)
  case object Numpad9 extends Key(VK_NUMPAD9)
  case object Multiply extends Key(VK_MULTIPLY)
  case object Add extends Key(VK_ADD)
  case object Separator extends Key(VK_SEPARATOR)
  case object Subtract extends Key(VK_SUBTRACT)
  case object Decimal extends Key(VK_DECIMAL)
  case object Divide extends Key(VK_DIVIDE)
  case object Delete extends Key(VK_DELETE)
  case object NumLock extends Key(VK_NUM_LOCK)
  case object ScrollLock extends Key(VK_SCROLL_LOCK)
  case object F1 extends Key(VK_F1)
  case object F2 extends Key(VK_F2)
  case object F3 extends Key(VK_F3)
  case object F4 extends Key(VK_F4)
  case object F5 extends Key(VK_F5)
  case object F6 extends Key(VK_F6)
  case object F7 extends Key(VK_F7)
  case object F8 extends Key(VK_F8)
  case object F9 extends Key(VK_F9)
  case object F10 extends Key(VK_F10)
  case object F11 extends Key(VK_F11)
  case object F12 extends Key(VK_F12)
  case object F13 extends Key(VK_F13)
  case object F14 extends Key(VK_F14)
  case object F15 extends Key(VK_F15)
  case object F16 extends Key(VK_F16)
  case object F17 extends Key(VK_F17)
  case object F18 extends Key(VK_F18)
  case object F19 extends Key(VK_F19)
  case object F20 extends Key(VK_F20)
  case object F21 extends Key(VK_F21)
  case object F22 extends Key(VK_F22)
  case object F23 extends Key(VK_F23)
  case object F24 extends Key(VK_F24)
  case object Printscreen extends Key(VK_PRINTSCREEN)
  case object Insert extends Key(VK_INSERT)
  case object Help extends Key(VK_HELP)
  case object BackQuote extends Key(VK_BACK_QUOTE)
  case object Quote extends Key(VK_QUOTE)
  case object KpUp extends Key(VK_KP_UP)
  case object KpDown extends Key(VK_KP_DOWN)
  case object KpLeft extends Key(VK_KP_LEFT)
  case object KpRight extends Key(VK_KP_RIGHT)
  case object DeadGrave extends Key(VK_DEAD_GRAVE)
  case object DeadAcute extends Key(VK_DEAD_ACUTE)
  case object DeadCircumflex extends Key(VK_DEAD_CIRCUMFLEX)
  case object DeadTilde extends Key(VK_DEAD_TILDE)
  case object DeadMacron extends Key(VK_DEAD_MACRON)
  case object DeadBreve extends Key(VK_DEAD_BREVE)
  case object DeadAbovedot extends Key(VK_DEAD_ABOVEDOT)
  case object DeadDiaeresis extends Key(VK_DEAD_DIAERESIS)
  case object DeadAbovering extends Key(VK_DEAD_ABOVERING)
  case object DeadDoubleacute extends Key(VK_DEAD_DOUBLEACUTE)
  case object DeadCaron extends Key(VK_DEAD_CARON)
  case object DeadCedilla extends Key(VK_DEAD_CEDILLA)
  case object DeadOgonek extends Key(VK_DEAD_OGONEK)
  case object DeadIota extends Key(VK_DEAD_IOTA)
  case object DeadVoicedSound extends Key(VK_DEAD_VOICED_SOUND)
  case object DeadSemivoicedSound extends Key(VK_DEAD_SEMIVOICED_SOUND)
  case object Ampersand extends Key(VK_AMPERSAND)
  case object Asterisk extends Key(VK_ASTERISK)
  case object Quotedbl extends Key(VK_QUOTEDBL)
  case object Less extends Key(VK_LESS)
  case object Greater extends Key(VK_GREATER)
  case object Braceleft extends Key(VK_BRACELEFT)
  case object Braceright extends Key(VK_BRACERIGHT)
  case object At extends Key(VK_AT)
  case object Colon extends Key(VK_COLON)
  case object Circumflex extends Key(VK_CIRCUMFLEX)
  case object Dollar extends Key(VK_DOLLAR)
  case object EuroSign extends Key(VK_EURO_SIGN)
  case object ExclamationMark extends Key(VK_EXCLAMATION_MARK)
  case object InvertedExclamationMark extends Key(VK_INVERTED_EXCLAMATION_MARK)
  case object LeftParenthesis extends Key(VK_LEFT_PARENTHESIS)
  case object NumberSign extends Key(VK_NUMBER_SIGN)
  case object Plus extends Key(VK_PLUS)
  case object RightParenthesis extends Key(VK_RIGHT_PARENTHESIS)
  case object Underscore extends Key(VK_UNDERSCORE)
  case object Windows extends Key(VK_WINDOWS)
  case object ContextMenu extends Key(VK_CONTEXT_MENU)
  case object Final extends Key(VK_FINAL)
  case object Convert extends Key(VK_CONVERT)
  case object Nonconvert extends Key(VK_NONCONVERT)
  case object Accept extends Key(VK_ACCEPT)
  case object Modechange extends Key(VK_MODECHANGE)
  case object Kana extends Key(VK_KANA)
  case object Kanji extends Key(VK_KANJI)
  case object Alphanumeric extends Key(VK_ALPHANUMERIC)
  case object Katakana extends Key(VK_KATAKANA)
  case object Hiragana extends Key(VK_HIRAGANA)
  case object FullWidth extends Key(VK_FULL_WIDTH)
  case object HalfWidth extends Key(VK_HALF_WIDTH)
  case object RomanCharacters extends Key(VK_ROMAN_CHARACTERS)
  case object AllCandidates extends Key(VK_ALL_CANDIDATES)
  case object PreviousCandidate extends Key(VK_PREVIOUS_CANDIDATE)
  case object CodeInput extends Key(VK_CODE_INPUT)
  case object JapaneseKatakana extends Key(VK_JAPANESE_KATAKANA)
  case object JapaneseHiragana extends Key(VK_JAPANESE_HIRAGANA)
  case object JapaneseRoman extends Key(VK_JAPANESE_ROMAN)
  case object KanaLock extends Key(VK_KANA_LOCK)
  case object InputMethodOnOff extends Key(VK_INPUT_METHOD_ON_OFF)
  case object Cut extends Key(VK_CUT)
  case object Copy extends Key(VK_COPY)
  case object Paste extends Key(VK_PASTE)
  case object Undo extends Key(VK_UNDO)
  case object Again extends Key(VK_AGAIN)
  case object Find extends Key(VK_FIND)
  case object Props extends Key(VK_PROPS)
  case object Stop extends Key(VK_STOP)
  case object Compose extends Key(VK_COMPOSE)
  case object Begin extends Key(VK_BEGIN)
  case object Undefined extends Key(VK_UNDEFINED)

  val values: List[Key] = List(
    Shift, Control, Alt, AltGraph, Meta, Enter, BackSpace, Tab, Cancel,
    Clear, Pause, CapsLock, Escape, Space, PageUp, PageDown, End, Home,
    Left, Up, Right, Down, Comma, Minus, Period, Slash, Key0,
    Key1, Key2, Key3, Key4, Key5, Key6, Key7, Key8, Key9,
    Semicolon, Equals, A, B, C, D, E, F, G, H, I, J, K, L, M,
    N, O, P, Q, R, S, T, U, V, W, X, Y, Z, OpenBracket, BackSlash,
    CloseBracket, Numpad0, Numpad1, Numpad2, Numpad3, Numpad4,
    Numpad5, Numpad6, Numpad7, Numpad8, Numpad9, Multiply,
    Add, Separator, Subtract, Decimal, Divide, Delete,
    NumLock, ScrollLock, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10,
    F11, F12, F13, F14, F15, F16, F17, F18, F19, F20, F21, F22,
    F23, F24, Printscreen, Insert, Help, BackQuote, Quote, KpUp, KpDown,
    KpLeft, KpRight, DeadGrave, DeadAcute, DeadCircumflex, DeadTilde,
    DeadMacron, DeadBreve, DeadAbovedot,
    DeadDiaeresis, DeadAbovering, DeadDoubleacute,
    DeadCaron, DeadCedilla, DeadOgonek,
    DeadIota, DeadVoicedSound, DeadSemivoicedSound,
    Ampersand, Asterisk, Quotedbl, Less, Greater, Braceleft,
    Braceright, At, Colon, Circumflex, Dollar, EuroSign,
    ExclamationMark, InvertedExclamationMark, LeftParenthesis,
    NumberSign, Plus, RightParenthesis,
    Underscore, Windows, ContextMenu, Final, Convert, Nonconvert,
    Accept, Modechange, Kana, Kanji, Alphanumeric, Katakana,
    Hiragana, FullWidth, HalfWidth,
    RomanCharacters, AllCandidates, PreviousCandidate,
    CodeInput, JapaneseKatakana, JapaneseHiragana,
    JapaneseRoman, KanaLock, InputMethodOnOff,
    Cut, Copy, Paste, Undo, Again, Find, Props, Stop, Compose,
    Begin, Undefined
  )
 
  val get: Map[Int,Key] = values map { k ⇒ k.v → k } toMap

  implicit val KeyEqual = Equal.equalA[Key]
}

// vim: set ts=2 sw=2 et:
