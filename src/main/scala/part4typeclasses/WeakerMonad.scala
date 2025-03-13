package part4typeclasses

import cats.Apply

object WeakerMonad {

  import cats.Applicative

  trait MyFlatMap[M[_]] extends Apply[M]{
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    def ap[A, B](wf: M[A => B])(wa: M[A]): M[B] =
      flatMap(wa)(a => map(wf)(f => f(a)))
  }


  trait MyMonad[M[_]] extends Applicative[M] with MyFlatMap[M]{
  }

  import cats.FlatMap
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def getPair[M[_]: FlatMap, A, B](numbers: M[A], chars: M[B]): M[(A, B)] = for {
    n <- numbers
    c <- chars
  } yield (n, c)

}
