package part3datamanipulation

object Evaluation {
  /*
    Cats makes the distinction between
      - evaluating an expression eagerly
      - evaluating lazily and every time you request it
      - evaluating lazily and keeping the value (memoization)
   */

  import cats.Eval
  val instantEval: Eval[Int] = Eval.now {
    println("Computing now!")
    1
  }

  val redoEval = Eval.always {
    println("Computing again!")
    2
  }

  val delayEval = Eval.later {
    println("Computing later!")
    3
  }

  val composedEvaluation = instantEval.flatMap(value1 => delayEval.map(value2 => value1 + value2))
  val anotherComposedEvaluation = for {
    value1 <- instantEval
    value2 <- delayEval
  } yield value1 + value2

  val evalEx1 = for {
    a <- delayEval
    b <- redoEval
    c <- instantEval
    d <- redoEval
  } yield a + b + c + d

  // "remember" a computed value
  val dontRecompute = redoEval.memoize

  val tutorial = Eval
    .always { println("Step 1..."); "put the guitar on your lap"}
    .map {step1 => println("Step 2..."); s"$step1 then put your left hand on the neck"}
    .memoize // remember the value up to this point
    .map {steps12 => println("Step 3, more complicated"); s"$steps12 then with the right hand strike the strings"}

  /*
    Exercise
      1. implement defer such that defer(Eval.now) does not run the side effects
   */

  def defer[T](eval: => Eval[T]): Eval[T] = Eval.later(()).flatMap(_ => eval)

  /*
    Exercise
      2. Rewrite "reverseList" method with Evals
   */
  def reverseList[T](list: List[T]): List[T] = {
    if (list.isEmpty) list
    else reverseList(list.tail) :+ list.head
  }

  def reverseEval[T](list: List[T]): Eval[List[T]] = {
    if (list.isEmpty) Eval.now(list)
    else defer(reverseEval(list.tail).map(_ :+ list.head))
  }

  def main(args: Array[String]): Unit = {
//    println(redoEval.value)
//    println(redoEval.value)
//    println(delayEval.value)
//    println(delayEval.value)

//    println(composedEvaluation.value)
//    println(anotherComposedEvaluation.value)

//    println(evalEx1.value)
//    println(evalEx1.value)

    /*
      Computing now!
      Computing later!
      Computing again!
      Computing again!
      8
      Computing again!
      Computing again!
      8
     */

    println(tutorial.value)
    println(tutorial.value)

    val deferEval = defer {
      Eval.now {
        println("Now")
        42
      }
    }

    println(deferEval.value)

    println(reverseEval((1 to 10000).toList).value)
  }
}
