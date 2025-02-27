package part1intro

object CatsIntro {

  // Eq
  val aComparison = 2 == "a string"

  // part 1 - type class import
  import cats.Eq

  // part 2 - import TC instances for the types you need
  // import cats.instances.int._

  // part 3 - use the TC API
  val intEquality = Eq[Int]
  val aTypeSafeComparision = intEquality.eqv(2, 3)
  // val anUnSafeComparision = intEquality.eqv(2, "a string") // does not compile

  // part  4 - use extension methods (if applicable)
  import cats.syntax.eq._
  val anotherTypeSafeComp = 2 === 3 // false
  val neqComparison = 2 =!= 3 // true
  // val invalidComparison = 2 === "string" // doesn't compile

  // part 5 - extending the type class operations to composite types, e.g. lists
  val aListComparision: Boolean = List("hello", "world") === List("hello", "world") // true

  // part 6 - create a TC instance for a custom type
  case class ToyCar(model: String, price: Double)

  implicit val toyCarEq: Eq[ToyCar] = (car1, car2) => car1.price == car2.price

  val aToyCarsComparision = ToyCar("n1", 12) =!= ToyCar("n1", 12) // false

  def main(args: Array[String]): Unit = {
  }
}
