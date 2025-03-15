package part4typeclasses

import cats.{Applicative, Monad, MonadError}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object HandlingErrors {

  trait MyApplicativeError[M[_], E] extends Applicative[M] {
    // pure from Applicative
    def raiseError[A](e: E): M[A]
    def handleErrorWith[A](ma: M[A])(func: E => M[A]): M[A]
    def handleError[A](ma: M[A])(func: E => A): M[A] = handleErrorWith(ma)(e => pure(func(e)))
  }

  trait MyMonadError[M[_], E] extends MyApplicativeError[M, E] with Monad[M] {
    def ensure[A](ma: M[A])(error: E)(predicate: A => Boolean): M[A]
  }

  // monads => MonadError

  import cats.MonadError

  type ErrorOr[A] = Either[String, A]

  val success: ErrorOr[Int] = MonadError[ErrorOr, String].pure[Int](32) // Right(32)
  val failure: ErrorOr[Int] = MonadError[ErrorOr, String].raiseError[Int]("something wrong") // Left("something wrong")

  // "recover"
  val handleError: ErrorOr[Int] = MonadError[ErrorOr, String].handleError(failure) {
    case "badness" => 44
    case _ => 98
  }

  // "recoverWith"
  val handleErrorWith: ErrorOr[Int] = MonadError[ErrorOr, String].handleErrorWith(failure) {
    case "badness" => MonadError[ErrorOr, String].pure(42)
    case _ => Left("something else")
  }

  // "filter"
  val filteredSuccess: ErrorOr[Int] = MonadError[ErrorOr, String].ensure(success)("number too small")(_ > 100)


  // Try and Future
  val exception = new RuntimeException("really bad exception")
  val pureException: Try[Int] = MonadError[Try, Throwable].raiseError(exception) // Failure(exception)

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  MonadError[Future, Throwable].raiseError(exception) // Future which will complete with a Failure(exception)

  // applicatives => ApplicativeError

  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]

  import cats.ApplicativeError

  val applicativeErrorVal = ApplicativeError[ErrorsOr, List[String]]
  // pure, raiseError, handleError, handleErrorWith

  // extension methods
  import cats.syntax.applicative._ // pure
  import cats.syntax.applicativeError._ // raiseError, handleError(With)
  val extendedSuccess: ErrorsOr[Int] = 42.pure[ErrorsOr] // requires the implicit ApplicativeError[ErrorsOr, List[String]]
  val extendedError: ErrorsOr[Int] = List("badness").raiseError[ErrorsOr, Int]
  val recoveredError: ErrorsOr[Int] = extendedError.recover {
    case _ => 42
  }

  import cats.syntax.monadError._ // ensure

  val testedSuccess: ErrorOr[Int] = success.ensure("something bad")(_ > 100)

  def main(args: Array[String]): Unit = {

  }
}
