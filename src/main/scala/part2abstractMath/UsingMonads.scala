package part2abstractMath

object UsingMonads {

  val aManualEither: Either[String, Int] = Right(42)

  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]

  import cats.Monad

  val anEither = Monad[LoadingOr].pure(45)
  val aChangedLoading = Monad[LoadingOr].flatMap(anEither)(n => if (n % 2 == 0) Right(n  + 1) else Left("Loading meaning of Life ..."))

  // imaginary online store
  case class OrderStatus(orderId: Long, status: String)
  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] =
    Right(OrderStatus(orderId, "Ready to Ship"))

  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.orderId > 1000) Left("Not available yet, refreshing data ...")
    else Right("Amsterdam, NL")

  val orderId = 475L
  val orderLocation: LoadingOr[String] = Monad[LoadingOr].flatMap(getOrderStatus(orderId))(trackLocation)

  // use extension methods
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  val orderLocationBetter: LoadingOr[String] = getOrderStatus(orderId).flatMap(trackLocation)
  val orderLocationFor: LoadingOr[String] = for {
    orderStatus <- getOrderStatus(orderId)
    orderLocation <- trackLocation(orderStatus)
  } yield orderLocation

  /*
    Exercise
      the service layer API of a web app
   */
  case class Connection(host: String, port: String)
  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]
  }

  def getResponse[M[_] : Monad](service: HttpService[M], payload: String): M[String] =
    for {
      conn <- service.getConnection(config)
      response <- service.issueRequest(conn, payload)
    } yield response

  // DO NOT CHANGE THE CODE ABOVE

  /*
    Requirements:
      - if the host and port are found in the configuration map, then we'll
        return a connection with those values
        otherwise, the method will fail,according to the logic of type M
        (for Try it will return a Failure, for Option it will return None,
         for Future it will be a failed Future, for Either it will be Left)
      - the issueRequest method returns a M containing the string:
        "request (payload) has accepted", if the payload is less than 20 characters
        otherwise, the method will fail, according to the logic of type M

    provide a real implementation of HttpService using Try, Option, Future, Either
   */

  object OptionHttpService extends HttpService[Option] {

    override def getConnection(cfg: Map[String, String]): Option[Connection] =
      for {
        h <- cfg.get("host")
        p <- cfg.get("port")
      } yield Connection(h, p)

    override def issueRequest(connection: Connection, payload: String): Option[String] =
      if (payload.length < 20) Some(s"payload ($payload) has been accepted") else None
  }

  val responseOption = OptionHttpService.getConnection(config).flatMap {
    conn => OptionHttpService.issueRequest(conn, "Hello, HttpService")
  }

  val responseOptionFor =
    for {
      conn <- OptionHttpService.getConnection(config)
      response <- OptionHttpService.issueRequest(conn, "Hello, HttpService")
    } yield response

  object AggressiveHttpService extends HttpService[ErrorOr] {

    override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] =
      if (!cfg.contains("host") || !cfg.contains("port")) {
        Left(new RuntimeException("Connection could not be established: invalid configuration"))
      } else {
        Right(Connection(cfg("host"), cfg("port")))
      }

    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] =
      if (payload.length < 20) Right(s"Request ($payload) was accepted")
      else Left(new RuntimeException("Payload is too large"))
  }

  val errorResponse = for {
    conn <- AggressiveHttpService.getConnection(config)
    response <- AggressiveHttpService.issueRequest(conn, "Hello ErrorOr")
  } yield response

  def main(args: Array[String]): Unit = {
    println(getResponse(OptionHttpService, "Hello, HttpService"))
    println(getResponse(AggressiveHttpService, "Hello ErrorOr"))
  }
}
