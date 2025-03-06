package part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object MonadTransformers {
  def sumAllOptions(values: List[Option[Int]]): Int = ???

  // Monad Transformers - Higher-kinded types for convenience over nested monadic values

  // option transformer

  import cats.data.OptionT

  val listOfNumberOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
  val listOfCharOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), Option.empty[Char]))
  val listOfTupleOptions: OptionT[List, (Int, Char)] = for {
    char <- listOfCharOptions
    number <- listOfNumberOptions
  } yield (number, char)

  // either transformer

  import cats.data.EitherT
  val listOfEithers: EitherT[List, String, Int] = EitherT(List(Left("something wrong"), Right(32), Right(2)))
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val futureOfEither: EitherT[Future, String, Int] = EitherT(Future[Either[String, Int]](Right(45)))

  /*
    Exercise
      we have a multi-machine cluster for your business which will receive a traffic surge following a media
      appearance.
      We measure bandwidth in units
      we want to allocate two of our servers to cope with the traffic spike
      We know the current capacity for each server, and we know we'll hold the traffic if the sum of bandwidths is > 250
   */

  val bandwidths = Map(
    "server1.rockthejvm.com" -> 50,
    "server2.rockthejvm.com" -> 300,
    "server3.rockthejvm.com" -> 170
  )

  type AsyncResponse[T] = EitherT[Future, String, T] // wrapper over Future[Either[String, T]]

  def getBandwidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
    case Some(value) => EitherT(Future[Either[String, Int]](Right(value)))
    case None => EitherT(Future[Either[String, Int]](Left(s"server $server unreachable")))
  }

  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = for {
    b1 <- getBandwidth(s1)
    b2 <- getBandwidth(s2)
  } yield b1 + b2 > 250

  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] =
    canWithstandSurge(s1, s2).transform {
      case Left(reason) => Left(s"$s1 and $s2 cannot withstand the traffic: $reason")
      case Right(false) => Left(s"$s1 and $s2 cannot withstand the traffic: not enough total bandwidth")
      case Right(true) => Right(s"$s1 and $s2 can withstand the traffic")
    }

  def main(args: Array[String]): Unit = {
    println(listOfTupleOptions.value) // List(Some((1,a)), Some((2,a)), Some((1,b)), Some((2,b)), None)
    generateTrafficSpikeReport("server2.rockthejvm.com", "server42").value.foreach(println)
    generateTrafficSpikeReport("server1.rockthejvm.com", "server3.rockthejvm.com").value.foreach(println)
    generateTrafficSpikeReport("server1.rockthejvm.com", "server2.rockthejvm.com").value.foreach(println)
  }

}
