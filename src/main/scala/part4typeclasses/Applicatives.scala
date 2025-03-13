package part4typeclasses

object Applicatives {
  // Applicatives = Functors + the pure method
  import cats.Applicative

  val aList: List[Int] = Applicative[List].pure(2) // List(2)
  val anOption: Option[Int] = Applicative[Option].pure(2) // Some(2)

  // pure extension method
  import cats.syntax.applicative._

  val aSweetList: List[Int] = 2.pure[List]
  val aSweetOption: Option[Int] = 2.pure[Option]

  // Monads extend Applicatives
  // Applicatives extend Functors

  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]

  val aValidValue: ErrorsOr[Int] = Validated.valid(43)
  val aModifiedValidated: ErrorsOr[Int] = aValidValue.map(_ + 1)

  val validatedApplicative = Applicative[ErrorsOr]

  /*
    Exercise
      1. product with Applicatives
   */
  // def ap[W[_], B, T](wf: W[B => T])(wb: W[B]): W[T] = ??? // already implemented
  def productWithApplicatives[W[_], A, B](wa: W[A], wb: W[B])(implicit applicative: Applicative[W]): W[(A, B)] = {
    val functionWrapper: W[B => (A, B)] = applicative.map(wa)(a => (b: B) => (a, b))
    applicative.ap(functionWrapper)(wb)
  }

  // Applicatives extend Semigroupals

  def main(args: Array[String]): Unit = {

  }
}
