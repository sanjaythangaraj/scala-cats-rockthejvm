package part2abstractMath

object Monoids {
  import cats.Semigroup
  import cats.syntax.semigroup._

  val numbers = (1 to 1000).toList
  // |+| is always associative

  val sumLeft = numbers.foldLeft(0)(_ |+| _) // 500500
  val sumRight = numbers.foldRight(0)(_ |+| _) // 500500

  // define a general API

//  def combineFold[T: Semigroup](list: List[T]): T =
//    list.foldLeft(/* WHAT? */)(_ |+| _)

  // MONOIDS

  import cats.Monoid

  def combineFold[T: Monoid](list: List[T]): T =
    list.foldLeft(Monoid[T].empty)(_ |+| _)

  case class Expense(id: Long, amount: Double)

  implicit val expenseMonoid: Monoid[Expense] = new Monoid[Expense] {
    override def empty: Expense = Expense(-1, 0)
    override def combine(e1: Expense, e2: Expense): Expense =
      Expense(if (e1.id > e2.id) e1.id else e2.id, e1.amount + e2.amount)
  }

  /*
    Exercise
      1. combine a list of phonebooks as Map[String, Int]
   */

  val phoneBooks = List(
    Map("Alice" -> 235, "Bob" -> 647),
    Map("Charlie" -> 372, "Daniel" -> 889),
    Map("Tina" -> 123)
  )


// no need to construct Monoid

//  implicit val mapMonoid: Monoid[Map[String, Int]] = new Monoid[Map[String, Int]] {
//    override def empty: Map[String, Int] = Map()
//    override def combine(x: Map[String, Int], y: Map[String, Int]): Map[String, Int] = x ++ y
//  }

//implicit def mapMonoid[K, V]: Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
//  override def empty: Map[K, V] = Map.empty
//  override def combine(x: Map[K, V], y: Map[K, V]): Map[K, V] = x ++ y
//}

  /*
    Exercise
      2. aggregate a list of shopping carts into one with monoid
   */
  case class ShoppingCart(items: List[String], total: Double)

  implicit val shoppingCartMonoid: Monoid[ShoppingCart] = Monoid.instance(
    ShoppingCart(List(), 0),
    (x, y) => ShoppingCart(x.items ++ y.items, x.total + y.total)
  )

  def checkout(list: List[ShoppingCart]): ShoppingCart = combineFold(list)


  def main(args: Array[String]): Unit = {
    println(combineFold[Expense](List(Expense(3, 45.25), Expense(7, 34.75), Expense(1, 20.00))))
    println(combineFold(List(Option("Hello "), Option.empty, Option("World"))))
    println(combineFold(phoneBooks))
    println(checkout(List(
      ShoppingCart(List("smartphone", "shoes"), 799),
      ShoppingCart(List("tv"), 20000),
      ShoppingCart(List(), 0)
    )))
  }
}
