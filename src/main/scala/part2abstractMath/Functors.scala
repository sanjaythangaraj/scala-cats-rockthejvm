package part2abstractMath

import scala.util.Try

object Functors {

  val aModifiedList = List(1, 2, 3).map(_ + 1) // List(2, 3, 4)
  val aModifiedOption = Option(2).map(_ + 1) // Some(3)
  val aModifiedTry = Try(42).map(_ + 1) // Success(43)

  // simplified definition
  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  import cats.Functor

  val incrementedNumbers = Functor[List].map(List(1, 2, 3))(_ + 1) // List(2, 3, 4)
  val incrementedOptions = Functor[Option].map(Option(2))(_ + 1) // Some(3)
  val incrementedTry = Functor[Try].map(Try(42))(_ + 1) // Success(43)

  // generalising an API
  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)
  def do10xOption(option: Option[Int]): Option[Int] = option.map(_ * 10)
  def do10xTry(theTry: Try[Int]): Try[Int] = theTry.map(_ * 10)

  // generalize
  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(container)(_ * 10)

  /*
    Exercise
      1. define your own functor for a binary tree
   */

  trait Tree[+T]
  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(value) => Leaf(f(value))
      case Branch(value, left, right) => Branch(f(value), map(left)(f), map(right)(f))
    }
  }

  import cats.syntax.functor._

  val tree: Tree[Int] = Branch(1, Branch(2, Leaf(3), Leaf(4)), Leaf(5))
  val incrementedTree = tree.map(_ + 1)

  /*
    Exercise:
      2. write a shorter do10x method using extension methods
   */

  def do10x_v2[F[_] : Functor](container: F[Int]): F[Int] = container.map(_ * 10)

  def main(args: Array[String]): Unit = {
    println(do10x(List(1, 2, 3)))
    println(do10x(Option(2)))
    println(do10x(Try(42)))

    println(do10x_v2[Tree](Branch(1, Branch(2, Leaf(3), Leaf(4)), Leaf(5))))
    println(incrementedTree)

  }

}
