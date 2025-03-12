package part3datamanipulation

object FunctionalState {
  type MyState[S, A] = S => (S, A)


  import cats.data.State

  val countAndSay: State[Int, String] = State(currentCount => (currentCount + 1, s"Counted $currentCount"))
  val (eleven, counted10) = countAndSay.run(10).value

  // state = "iterative" computation

  // iterative
  var a = 10
  a += 1
  val firstComputation = s"Added 1 to 10, obtained $a"
  a *= 5
  val secondComputation = s"Multiplied with 5, obtained $a"

  // pure FP with states
  val firstTransformation = State((s: Int) => (s+1, s"Added 1, obtained $s"))
  val secondTransformation = State((s: Int) => (s*5, s"Multiplied with 5, obtained $s"))

  val compositeTransformation = firstTransformation.flatMap{ firstResult =>
    secondTransformation.map(secondResult => (firstResult, secondResult))
  }

  val compositeTransformation2 = for {
    firstResult <- firstTransformation
    secondResult <- secondTransformation
  } yield (firstResult, secondResult)

  // Why not compose functions instead? func composition is clunky

  val func1 = (s: Int) => (s+1, s"Added 1, obtained ${s + 1}")
  val func2 = (s: Int) => (s*5, s"Multiplied with 5, obtained ${s * 5}")

  val compositeFunc = func1.andThen {
    case (newState,firstResult) => (firstResult, func2(newState))
  }

  case class ShoppingCart(items: List[String], total: Double)
  def addToCart(item: String, price: Double): State[ShoppingCart, Double] = State { cart =>
    (ShoppingCart(item :: cart.items, cart.total + price), price + cart.total)
  }

  val danielsCart: State[ShoppingCart, Double] = for {
    _ <- addToCart("Fender Guitar", 500)
    - <- addToCart("Elixir strings", 19)
    total <- addToCart("Electric cable", 8)
  } yield total

  /*
    Exercise
      implement the below methods
   */

  // returns a State data structure that, when run, will not change the state but will issue the value f(a)
  def inspect[A, B](f: A => B): State[A, B] = State (a => (a, f(a)))

  // returns a State data structure that, when run, returns the value of that state and makes no changes
  def get[A]: State[A, A] = State(a => (a, a))

  // returns a State data structure that, when run, returns Unit and sets the state to the value
  def set[A](value: A): State[A, Unit] = State(_ => (value, ()))

  // returns a State data structure that, when run, will return Unit and sets the state to f(state)
  def modify[A](f: A => A): State[A, Unit] = State(a => (f(a), ()))

  // methods available
  import cats.data.State._

  val program: State[Int, (Int, Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 10) // state = a + 10
    b <- get[Int] // b = a + 10
    c <- inspect[Int, Int](_ * 20) // c = b * 20
    _ <- modify[Int](_ + 20) // state = b + 20
    d <- get[Int]
  } yield(a, b, c, d)

  def main(args: Array[String]): Unit = {
    println(compositeTransformation.run(10).value) // (55,(Added 1, obtained 10,Multiplied with 5, obtained 11))
    println(compositeFunc(10))                     // (Added 1, obtained 11,(55,Multiplied with 5, obtained 55))

    println(danielsCart.run(ShoppingCart(List(), 0)).value)

    println(program.run(10).value) // (10, 20, 400, 40)
  }
}
