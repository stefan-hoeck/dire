package dire

import scalaz._, Scalaz._, effect.IO

/** Helper functions for dealing with input validation */
trait ValidationFunctions {
  import validation.{ValRes, DisRes, Validator, ValidatorIO}
  type Errors = NonEmptyList[String]

  /** A validated value */
  type ValRes[A] = Validation[Errors,A]

  /** A validated value represented as a disjunction */
  type DisRes[A] = Errors \/ A

  /** An IO action returning a disjunction */
  type DisIO[A] = EitherT[IO,Errors,A]

  /** Reads an input value of type `A` and returns a 
    * validated value of type `B`.
    */
  type Validator[A,B] = Kleisli[DisRes,A,B]

  /** Reads an input value of type `A` and returns a 
    * validated value of type `B` in an IO action.
    */
  type ValidatorIO[A,B] = Kleisli[DisIO,A,B]

  /** Signal function of a validated value */
  type SfV[A,B] = SF[A,ValRes[B]]

  /** Input signal of a validated value */
  type VSIn[A] = SIn[ValRes[A]]

  def point[A,B](b: ⇒ B): SfV[A,B] = validation.SfVApplicative[A] point b

  /** Returns a signal function that takes some validated input
    * and performs some additional validation.
    *
    * Note that errors are not accumulated in this case, since the
    * two validation steps happen sequentially.
    */
  def reValidate[A,B](v: Validator[A,B]): SfV[ValRes[A],B] =
    SF.id[ValRes[A]] map { _ @\/ { _ >>= v.run } }

  /** Returns a signal faction that turns all input into a success */
  def success[A]: SfV[A,A] = SF.id[A] map { _.success }

  /** Returns a signal function that validates its input */
  def validate[A,B](v: Validator[A,B]): SfV[A,B] =
    SF.id[A] map { v run _ validation }

  /** Turns an input signal function into a validated input
    * signal function that fires only successes.
    */
  def valid[A](in: SIn[A]): VSIn[A] = in >=> success[A]

  /** Returns a signal function that validates its input by
    * running an IO action.
    *
    * Note that the validation step blocks the main reactor thread,
    * therefore it should be reasonably fast.
    */
  def validateIO[A,B](v: ValidatorIO[A,B]): SfV[A,B] =
    SF syncIO { v.run(_).run map { _.validation } }

  /** Lifts a value into a pure validated input signal */
  def vsin[A](a: ⇒ A): VSIn[A] = point(a)
}

/** Type class instances for signal functions that
  * deal with validated input */
trait ValidationInstances {
  import validation.{SfV, ValRes}

  implicit def SfVApplicative[R]: Applicative[({type λ[α]=SfV[R,α]})#λ] =
    Applicative[({type λ[α]=SF[R,α]})#λ].compose[ValRes]
}

object validation extends ValidationFunctions with ValidationInstances

// vim: set ts=2 sw=2 et:
