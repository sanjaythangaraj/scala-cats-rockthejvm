package part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Monads {

  // lists
  val numbersList = List(1, 2, 3)
  val charsList = List('a', 'b', 'c')

  val combinationsList = numbersList.flatMap(n => charsList.map(c => (n, c)))
  val combinationsListFor = for {
    n <- numbersList
    c <- charsList
  } yield (n, c)

  // options
  val numberOption = Option(2)
  val charOption = Option('d')

  val combinationOption = numberOption.flatMap(n => charOption.map(c => (n, c)))
  val combinationOptionFor = for {
    n <- numberOption
    c <- charOption
  } yield (n,c)

  // futures
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))
  val numberFuture = Future(42)
  val charFuture = Future('Z')

  val combinationFuture = numberFuture.flatMap(n => charFuture.map(c => (n ,c)))
  val combinationFutureFor = for {
    n <- numberFuture
    c <- charFuture
  } yield (n, c)

  /*
    Pattern
      - wrapping a value into a monadic value
      - the flatMap mechanism

    MONADS
   */

  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    /*
      Exercise
        1. implement map with flatMap and pure

        // Monads extend Functors
     */
    def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(a => pure(f(a)))
  }

  // cats monad
  import cats.Monad

  val anOption = Monad[Option].pure(4) // Option(4)
  val aTransformedOption = Monad[Option].flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None)

  val aList = Monad[List].pure(3) // List(3)
  val aTransformedList = Monad[List].flatMap(aList)(x => List(x , x + 1)) // List(4, 5)

  val aFuture = Monad[Future].pure(42)
  val aTransformedFuture = Monad[Future].flatMap(aFuture)(x => Future(x + 1)) // Future(43)

  // specialized API
  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] = numbers.flatMap(n => chars.map(c => (n, c)))
  def getPairsOption(number: Option[Number], char: Option[Char]) = number.flatMap(n => char.map(c => (n, c)))
  def getPairsFuture(number: Future[Number], char: Future[Char]) = number.flatMap(n => char.map(c => (n, c)))

  // generalize
  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] =
    monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))

  // extension methods - weirder imports - pure, flatMap
  import cats.syntax.applicative._ // pure is here

  val oneOption: Option[Int] = 1.pure[Option]
  val oneList: List[Int] = 1.pure[List]

  import cats.syntax.flatMap._ // flatMap is here
  oneOption.flatMap(x => (x + 1).pure[Option])

  import cats.syntax.functor._

  oneOption.map(_ + 2)
  Monad[Option].map(oneOption)(_ + 2)

  // for-comprehensions
  val composedOptionFor = for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield one + two

  /*
    Exercise
      2. implement a shorter version of getPairs using for-comprehensions
   */
  def getPairs_v2[M[_] : Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] =
    for {
      a <- ma
      b <- mb
    } yield (a, b)

  def main(args: Array[String]): Unit = {
    println(getPairs(numbersList, charsList))
    println(getPairs_v2(numberOption, charOption))
    getPairs_v2(numberFuture, charFuture).foreach(println)
  }

}
