package part4typeclasses

import cats.{Functor, Semigroupal}

object WeakerApplicatives {

  trait MyApply[W[_]] extends Functor[W] with Semigroupal[W] {

    def ap[B, T](wf: W[B => T])(wb: W[B]): W[T] // fundamental

    override def product[A, B](fa: W[A], fb: W[B]): W[(A, B)] = {
      val functionWrapper: W[B => (A, B)] = map(fa)(a => (b: B) => (a, b))
      ap(functionWrapper)(fb)
    }

    // implement mapN
    def mapN[A, B, C](tuple: (W[A], W[B]))(f: (A, B) => C): W[C] = {
      val tupleWrapper = product(tuple._1, tuple._2) // W[(A, B)]
      map(tupleWrapper) {
        case (a, b) => f(a, b)
      }
    }

  }

  trait MyApplicative[W[_]] extends MyApply[W] {
    def pure[A](x: A): W[A] // fundamental
  }

  import cats.Apply

  val funcApp = Apply[Option].ap(Some((x: Int) => x + 1))(Some(2)) // Some(3)

  import cats.syntax.apply._

  val tupleOfOptions = (Option(1), Option(2), Option(3))
  val optionOfTuple = tupleOfOptions.tupled // Some((1, 2, 3))
  val sumOption = tupleOfOptions.mapN(_ + _ + _) // Some(6)
}
