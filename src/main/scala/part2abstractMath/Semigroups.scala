package part2abstractMath

object Semigroups {

  // Semigroups COMBINE elements of the same type
  import cats.Semigroup

  val intCombination = Semigroup[Int].combine(2, 46) // 48
  val stringCombination = Semigroup[String].combine("hello ", "world") // hello world

  def reduceInts(list: List[Int]): Int = list.reduce(Semigroup[Int].combine)
  def reduceStrings(list: List[String]): String = list.reduce(Semigroup[String].combine)

  def reduceThings[A: Semigroup](list: List[A]): A = list.reduce(Semigroup[A].combine)

  /*
    Exercise
      1. Suppose a new type
   */
  case class Expense(id: Long, amount: Double)

  implicit val expenseSemigroup: Semigroup[Expense] = (e1, e2)
    => Expense(if (e1.id > e2.id) e1.id else e2.id, e1.amount + e2.amount)

  // extension methods from Semigroup - |+|
  import cats.syntax.semigroup._
  val anIntSum = 2 |+| 3
  val aCombinedExpense = Expense(4, 80) |+| Expense(56, 40)

  /*
    Exercise
      2. implement reduceThings2 with the |+|
   */
  def reduceThings2[T: Semigroup](list: List[T]): T = list.reduce(_ |+| _)

  def main(args: Array[String]): Unit = {
    println(stringCombination) // hello world

    // specific API
    println(reduceInts((1 to 10).toList)) // 55
    println(reduceStrings(List("I ", "am ", "going ", " to", " sleep")))

    // general API
    println(reduceThings(List(Option("A"), Option("B")))) // Some(AB)

    println(reduceThings(List(Expense(3, 45.25), Expense(7, 34.75), Expense(1, 20.00)))) // Expense(7, 100.0)

    println(reduceThings2(List(Expense(3, 45.25), Expense(7, 34.75), Expense(1, 20.00))))
  }
}
