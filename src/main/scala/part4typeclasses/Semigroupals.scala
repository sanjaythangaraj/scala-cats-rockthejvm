package part4typeclasses

import cats.{Monad, Semigroup}
import cats.data.Validated

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Semigroupals {

  trait MySemigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  import cats.Semigroupal
  val aTupledOption = Semigroupal[Option].product(Some(123), Some("a string")) // Some((123,a string))
  val aNoneTupled = Semigroupal[Option].product(Some(123), None) // None

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))

  val aTupledFuture = Semigroupal[Future].product(Future("the meaning of life"), Future(42)) // Future(("the meaning of life", 42))

  val aTupledList = Semigroupal[List].product(List(1, 2), List("a", "b")) // List((1,a), (1,b), (2,a), (2,b))

  // implement product with monads

  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def productWithMonads_v0[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] =
    monad.flatMap(fa)(a => monad.map(fb)(b => (a, b)))

  def productWithMonads[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] = for {
    a <- fa
    b <- fb
  } yield (a, b)

  trait MyMonad[M[_]] extends MySemigroupal[M]{
    def pure[A](value: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    // Monads extend Functors

    def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(a => pure(f(a)))

    // Monads extend Semigroupals

    def product[A, B](fa: M[A], fb: M[B]): M[(A, B)] =
      flatMap(fa)(a => map(fb)(b => (a, b)))
  }

  type ErrorsOr[T] = Validated[List[String], T]

//  implicit def listSemigroup[A]: Semigroup[List[A]] = new Semigroup[List[A]] {
//    def combine(a: List[A], b: List[A]): List[A] = b
//  }

  // requires the implicit Semigroup[List[_]]
  val invalidsCombination: ErrorsOr[(Nothing, Nothing)] = Semigroupal[ErrorsOr].product(
    Validated.invalid(List("Something wrong", "Something else wrong")),
    Validated.invalid(List("This can't be right"))
  )

  type EitherErrorOr[T] = Either[List[String], T]
  // implicit Monad[Either]
  val eitherCombination: EitherErrorOr[(Nothing, Nothing)] = Semigroupal[EitherErrorOr].product( // in terms of map/flatMap
    Left(List("Something wrong", "Something else wrong")),
    Left(List("This can't be right"))
  )

  /*
    Exercise
      1. define a Semigroupal[List] which does a zip
   */

  val zipListSemigroupal: Semigroupal[List] = new Semigroupal[List] {

    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = fa.zip(fb)
  }

  def main(args: Array[String]): Unit = {
    println(aTupledOption)
    println(aNoneTupled)

    println(aTupledList)
    println(invalidsCombination)
    println(eitherCombination)

    println(zipListSemigroupal.product(List(1, 2), List("a", "b", "c")))
  }

}
