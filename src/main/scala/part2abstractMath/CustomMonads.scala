package part2abstractMath

import scala.annotation.tailrec

object CustomMonads {
  import cats.Monad
  implicit object OptionMonad extends Monad[Option] {

    override def pure[A](x: A): Option[A] = Option(x)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
      case Some(Left(value)) => tailRecM(value)(f)
      case Some(Right(value)) => Some(value)
      case None => None
    }
  }

  /*
    Exercise
      1. define a monad for the identity type
   */
    type Identity[T] = T
    val aNumber: Identity[Int] = 42

  implicit object IdentityMonad extends Monad[Identity] {
    override def pure[A](x: A): Identity[A] = x

    override def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa)

    override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = f(a) match {
      case Left(value) => tailRecM(value)(f)
      case Right(value) => value
    }
  }

  /*
    Exercise
      2. define a monad for the tree
   */

  sealed trait Tree[+A]
  final case class Leaf[+A](value: A) extends Tree[A]
  final case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  implicit object TreeMonad extends Monad[Tree] {

    override def pure[A](x: A): Tree[A] = Leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(value) => f(value)
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      def stackRec(t: Tree[Either[A, B]]): Tree[B] = t match {
        case Leaf(Left(value)) => stackRec(f(value))
        case Leaf(Right(value)) => Leaf(value)
        case Branch(left, right) => Branch(stackRec(left), stackRec(right))
      }

      stackRec(f(a))
    }
  }


  def main(args: Array[String]): Unit = {
    val tree: Tree[Int] = Branch(Leaf(10), Leaf(20))
    val changedTree = TreeMonad.flatMap(tree)(v => Branch(Leaf(v+1), Leaf(v+2)))
    println(changedTree)
  }
}