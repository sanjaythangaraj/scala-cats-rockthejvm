package part4typeclasses

import cats.{Eval, Monoid}

object Folding {

  // implement all in terms of foldLeft & foldRight
  object ListExercise {

    // f = x => x + 1
    // List(1, 2, 3).foldRight {
    // 1. (3, Nil) => f(3) :: Nil
    // 2. (2, f(3) :: Nil) => f(2) :: f(3) :: Nil
    // 3. (1, f(2) :: f(3) :: Nil) => f(1) :: f(2) :: f(3) :: Nil
    // } = List(2, 3, 4)
    def map[A, B](list: List[A])(f: A => B): List[B] =
      list.foldRight(List.empty[B])((element, accumulator) => f(element) :: accumulator)

    // f = x => List(x, x + 1)
    // List(1, 2, 3).foldLeft {
    // 1. (Nil, 1) => f(1)
    // 2. (List(1, 2), 2) => List(1, 2).foldRight {
    //    1. (2, f(2) =  List(2, 3))    => 2 :: List(2, 3)
    //    2. (1, List(2, 2, 3))         => 1 :: List(2, 2, 3)
    // }
    // 3. (List(1, 2, 2, 3), 3) => List(1,2,2,3).foldRight {
    //    1. (3, f(3))                      => 3 :: List(3, 4)
    //    2. (2, List(3, 3, 4))             => 2 :: List(3, 3, 4)
    //    3. (2, List(2, 3, 3, 4))          => 2 :: List(2, 3, 3, 4)
    //    4. (1, List(2, 2, 3, 3, 4))       => 1 :: List(2, 2, 3, 3, 4)
    // }
    // } = List(1, 2, 2, 3, 3, 4)
    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = {
      list.foldLeft(List.empty[B])((acc1, a) =>
        acc1.foldRight(f(a))((b, acc2) => b :: acc2))
    }

    // similar to map using foldRight
    def filter[A](list: List[A])(predicate: A => Boolean): List[A] = {
      list.foldRight(List.empty[A])((element, accumulator) =>
        if (predicate(element)) element :: accumulator else accumulator)
    }

    // List(1, 2, 3), let us assume |+| is addition and z is 0
    // List(1, 2, 3).foldLeft {
    //    1. (0, 1) => 0 + 1
    //    2. (1, 2) => 1 + 2
    //    3. (3, 3) => 3 + 3
    // } = 6
    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A = {
      import cats.syntax.monoid._
      list.foldLeft(monoid.empty)(_ |+| _)
    }

  }

  import cats.Foldable
  val sum: Int = Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _) // 6
  val sumOption: Int = Foldable[Option].foldLeft(Option(2), 30)(_ + _) // 32

  // foldRight is stack-safe regardless of your container
  val sumRight: Eval[Int] = Foldable[List].foldRight(List(1, 2, 3), Eval.now(0)) { (num, eval) =>
    eval.map(_  + num)
  }

  val anotherSum: Int = Foldable[List].combineAll(List(1, 2, 3)) // implicit Monoid[Int]
  val mappedConcat: String = Foldable[List].foldMap(List(1, 2, 3))(_.toString) // "123" // implicit Monoid[String]

  val intsNested = List(Vector(1, 2, 3), Vector(4, 5, 6))
  (Foldable[List] compose Foldable[Vector]).combineAll(intsNested) // implicit Monoid[Int]

  import cats.syntax.foldable._

  val sum3 = List(1, 2, 3).combineAll // requires Foldable[List], Monoid[Int]
  val mappedConcat2 = List(1, 2, 3).foldMap(_.toString)

  def main(args: Array[String]): Unit = {
    import ListExercise._
    println(map((1 to 3).toList)(_ + 1))
    println(flatMap((1 to 3).toList)(x => List(x, x + 1)))
    println(filter((1 to 10).toList)(_ % 2 == 0))
    println(combineAll((1 to 3).toList))
  }
}
