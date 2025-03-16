package part4typeclasses

import cats.{Applicative, Foldable, Monad}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Traversing {

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8));

  val servers: List[String] = List("server-ci.rockthejvm.com", "server-staging.rockthejvm.com", "prod.rockthejvm.com")

  def getBandwidth(hostname: String): Future[Int] = Future(hostname.length * 80)

  /*
    We have
      - a List[String]
      - a func String => Future[Int]
    We want a Future[List[Int]]
   */
  val allBandWidths: Future[List[Int]] = servers.foldLeft(Future(List.empty[Int])) { (accumulator, hostname) =>
    val bandwidthFuture: Future[Int] = getBandwidth(hostname)
    for {
      bandwidthsList <- accumulator
      bandwidth <- bandwidthFuture
    } yield bandwidth :: bandwidthsList
  }


  val allBandwidthsTraverse: Future[List[Int]] = Future.traverse(servers)(getBandwidth)
  val allBandwidthsSequence: Future[List[Int]] = Future.sequence(servers.map(getBandwidth))

  import cats.syntax.applicative._ // pure
  import cats.syntax.flatMap._ // flatMap
  import cats.syntax.functor._ // map
  import cats.syntax.apply._ // mapN

  def listTraverse_v0[F[_] : Monad, A, B](list: List[A])(func: A => F[B]): F[List[B]] = {
    list.foldLeft(List.empty[B].pure) { (wrappedAccumulator, element) =>
      val wrappedElement: F[B] = func(element)
      for {
        accumulator <- wrappedAccumulator
        element <- wrappedElement
      } yield accumulator :+ element
    }
  }

  // List(Vector(1, 2), Vector(3, 4)).foldLeft {
  //    1. (Vector(List()), Vector(1, 2)) => Vector(List(1), List(2))
  //    2. (Vector(List(1), List(2)), Vector(3, 4)) => Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4))
  // }
  // = Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4))
  val result: Vector[List[Int]] = listTraverse_v0(List(Vector(1, 2), Vector(3, 4)))(identity)

  def listTraverse[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] = {
    list.foldLeft(List.empty[B].pure) { (wrappedAccumulator, element) =>
      val wrappedElement: F[B] = func(element)
      (wrappedAccumulator, wrappedElement).mapN(_ :+ _)
    }
  }

  val result2: Vector[List[Int]] = listTraverse(List(Vector(1, 2), Vector(3, 4)))(identity)

  def listSequence[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] =
    listTraverse(list)(identity)

  listSequence(List(Vector(1, 2), Vector(3, 4))) // Vector[List[Int]] - all possible 2 pairs
  val result3 = listSequence(List(Vector(1,2), Vector(3, 4), Vector(5,6))) // Vector[List[Int]] = all possible 3 pairs

  def filterAsOption(list: List[Int])(predicate: Int => Boolean): Option[List[Int]] =
    listTraverse(list)(n => Some(n).filter(predicate))

  val allTrue = filterAsOption(List(2,4,6))(_ % 2 == 0) // Some(List(2, 4, 6))
  val someFalse = filterAsOption(List(1, 2, 3))(_ % 2 == 0) // None

  import cats.data.Validated

  type ErrorsOr[T] = Validated[List[String], T]

  def filterAsValidated(list: List[Int])(predicate: Int => Boolean): ErrorsOr[List[Int]] = {
    listTraverse(list)(n => if(predicate(n)) Validated.valid(n) else Validated.invalid(List(s"$n couldn't make it")))
  }

  val allTrueValidated = filterAsValidated(List(2, 4, 6))(_ % 2 == 0)
  val someFalseValidated = filterAsValidated(List(1, 2, 3))(_ % 2 == 0)

  trait MyTraverse[L[_]] extends Foldable[L] {
    def traverse[F[_]: Applicative, A, B](container: L[A])(func: A => F[B]): F[L[B]]
    def sequence[F[_]: Applicative, A](container: L[F[A]]): F[L[A]] =
      traverse(container)(identity)

//    type Identity[T] = T
//    def map[A, B](wa: L[A])(f: A => B): L[B] = traverse[Identity, A, B](wa)(f)

    import cats.Id
    def map[A, B](wa: L[A])(f: A => B): L[B] = traverse[Id, A, B](wa)(f)

  }

  import cats.Traverse

  val allBandwidthsCats = Traverse[List].traverse(servers)(getBandwidth)

  // extension methods
  import cats.syntax.traverse._

  val allBandwidthsCats2 = servers.traverse(getBandwidth)

  def main(args: Array[String]): Unit = {
    println(result)
    println(result2)
    println(result3)

    println(allTrue)
    println(someFalse)

    println(allTrueValidated) // Valid(List(2, 4, 6))
    println(someFalseValidated) // Invalid(List(1 couldn't make it, 3 couldn't make it))

    println(allBandwidthsCats2)
  }
}
