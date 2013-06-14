package dire.util

import dire.{SF, Out}, SF.{id, loop, syncIO, sf}
import scalaz._, Scalaz._, effect.IO

/** Provides utility signal functions for dealing with typical
  * scenarios when persisting user input.
  *
  * The functions and data types in this module where designed with
  * the following considerations in mind:
  *
  * Save functionality should typically be disabled if the actual
  * state of the data is the same as the state that was saved most
  * recently. We therefor keep track of the data object that was
  * last saved.
  *
  * Saving might be handled either by clicking buttons in a user interface
  * or entering the proper commands at the console (input events), or
  * by registering Saver objects at some central third-party save-handler
  * (for instance as `Savable` on the Netbeans platform). Both scenarios
  * should be supported here.
  */
object save {

  /** Type alias that represents an input behavior used for saving data.
    *
    * A Left represents a save event (the actual data has just been saved)
    * while a Right represents a change in the actual data (user input)
    */
  type SaveIn[+A] = Unit \/ A

  /** Type alias that represents the actually accumulated save info.
    *
    * The first value (_1) represents the state that was last saved,
    * the second (_2) the data's actual value.
    */
  type SaveOut[+A] = (Option[A], Option[A])

  /** A basic signal function for handling savable data
    *
    * By convention, input wrapped in a Right is assumed to come
    * from user input while input wrapped in a Left comes from
    * saving data.
    *
    * In client code the output of this function is typically looped back
    * to its input at some point.
    */
  def basic[A]: SF[SaveIn[A], SaveOut[A]] =
    sf(nextSt[A]) scanStS (none[A], none[A])

  /** Saving using a signal function representing a 'button' in a
    * user interface.
    *
    * Signal function `btn` should consume `Boolean` values indicating
    * whether the save button is enabled or disabled and should fire
    * `Unit` events whenever the button is clicked
    */
  def button[A:Equal](btn: SF[Boolean,Unit], save: Out[A])
    : SF[A,SaveOut[A]] = {
      val in = id[SaveOut[A]] collectO { _._2 }
      val btnA: SF[SaveOut[A],Unit] = btn ∙ { so ⇒ so._1 ≠ so._2 }
      val handler = in on btnA andThen syncIO(save)

      withHandler(handler)
    }

  /** Complete save logic using a handler for registering data
    * to be saved.
    *
    * Param `handle` collects SaveOut events and fires `Unit` whenever
    * data was safed. The resulting signal function still fires all
    * `SaveOut` events if they are needed by other parts of the reactive
    * graph.
    */
  def withHandler[A](handler: SF[SaveOut[A],Unit]): SF[A, SaveOut[A]] = {
    val in = id[SaveOut[A] \/ A]
    def safeIns = (in.collectL >=> handler) or in.collectR
    def safeOuts = basic[A] map { _.left[A] } 

    loop(safeIns >=> safeOuts).collectL ∙ { _.right }
  }

  //(s, oa): s = last saved (optional), oa: actual value (optional)
  //if in is Unit (= saved) the new state is (oa, oa)
  //if in is a (= new input) the new state is (Some(a), Some(a)) s was None
  //else it is (s, Some(a))
  //
  //if (s ≟ oa) this means that nothing changed since the last safe
  private def nextSt[A](in: SaveIn[A]): State[SaveOut[A],Unit] = 
    modify { case (s, oa) ⇒ 
      in fold (_ ⇒ (oa, oa), a ⇒ (s orElse Some(a), Some(a)))
    }
}

// vim: set ts=2 sw=2 et:
