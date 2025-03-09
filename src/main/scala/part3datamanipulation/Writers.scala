package part3datamanipulation

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Writers {

  import cats.data.Writer
  // Writer[logs type, value type]

  // 1. define them at the start
  val aWriter: Writer[List[String], Int] = Writer(List("started something"), 45)

  // 2. manipulate them with pure FP
  val anIncreasedWriter = aWriter.map(_ + 1) // value increases, logs stay the same
  val aLogsWriter = aWriter.mapWritten(_ :+ "found something interesting") // value stays the same, logs change
  val aWriterWithBoth = aWriter.bimap(_ :+ "found something interesting", _ + 1) // both value and logs change
  val aWriterWithBoth2 = aWriter.mapBoth {
    (log, value) => (log :+ "found something interesting", value + 1)
  }

  val writerA = Writer(Vector("Log A1", "Log A2"), 10)
  val writerB = Writer(Vector("Log B1"), 40)
  val compositeWriter = for {
    va <- writerA
    vb <- writerB
  } yield va + vb

  // reset the logs
  val anEmptyWriter = aWriter.reset // clear the log, keep the value

  // 3 - dump either the value or the log
  val desiredValue = aWriter.value
  val logs = aWriter.written
  val (l, v) = aWriter.run

  /*
    Exercise
      1. rewrite a function which "prints" things with writers
   */

  def countAndSay(n: Int): Unit = {
    if (n <= 0) println("starting")
    else {
      countAndSay(n - 1)
      println(n)
    }
  }

  def countAndLog(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector("starting"), 0)
    else {
      val writer = countAndLog(n - 1)
      writer.bimap(_ :+ n.toString, _ + 1)
    }
  }

  def countAndLog_v2(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector("starting"), 0)
    else {
      val writer = countAndLog(n - 1)
      writer.flatMap(_ => Writer(Vector(n.toString), n))
    }
  }

  // Benefit #1: we work with pure FP

  /*
    Exercise
      2. rewrite "naiveSum" with writers
   */

  def naiveSum(n: Int): Int = {
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val sum = naiveSum(n - 1)
      println(s"Computed Sum(${n-1}): =$sum")
      sum + n
    }
  }

  def sumWithLogs(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector(), 0)
    else {
      val writer = sumWithLogs(n - 1)
      writer.mapBoth((vector, value) => (s"Now at $n" +: vector :+  s"Computed Sum(${n-1}): =$value", value + n))
    }
  }

  def sumWithLogs_v2(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector(), 0)
    else for {
      _ <- Writer(Vector(s"Now at $n"), -1)
      sum <- sumWithLogs_v2(n-1)
      _ <- Writer(Vector(s"Computed Sum(${n-1}): =$sum"), -1)
    } yield sum + n
  }

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val sumFuture1 = Future(naiveSum(10)) // thread 1
  val sumFuture2 = Future(naiveSum(20)) // thread 2

  // logs from thread 1 and thread 2 are intertwined

  val sumWithLogsFuture1 = Future(sumWithLogs_v2(10))
  val sumWithLogsFuture2 = Future(sumWithLogs_v2(20))

  val logs1 = sumWithLogsFuture1.map(_.written) // logs from thread 1
  val logs2 = sumWithLogsFuture2.map(_.written) // logs from thread 2

  // Benefit #2: Writers can keep the logs separate from multiple threads

  def main(args: Array[String]): Unit = {
    println(compositeWriter.run)
    println(logs)
    println(anEmptyWriter)
    println(countAndLog_v2(10))
    println(sumWithLogs(10))
    println(sumWithLogs_v2(10))

    logs1.onComplete(println)
    logs2.onComplete(println)

  }
}
