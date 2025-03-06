package part3datamanipulation

object Readers {

  /*
    - configuration file => initial data structure
    - a DB layer
    - an HTTP layer
    - a business layer
   */

  case class Configuration(dbUser: String, dbPassword: String, host: String, port: Int, nThreads: Int, emailReplyFrom: String)
  case class DbConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String = "dispatched"
    def getLastOrderId(username: String): Long = 542643
  }
  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("server started")
  }

  // bootstrap
  val config = Configuration("daniel", "rockthejvm1!", "localhost", 1234, 8, "daniel@rockthejvm.com")

  import cats.data.Reader
  val dbReader: Reader[Configuration, DbConnection] = Reader(config => DbConnection(config.dbUser, config.dbPassword))
  val dbConnection = dbReader.run(config)

  val danielOrderStatusReader: Reader[Configuration, String] = dbReader.map(dbConn => dbConn.getOrderStatus(55))
  val danielOrderStatus: String = danielOrderStatusReader.run(config)

  def getLastOrderStatus(username: String): String = {
    val lastOrderStatusReader: Reader[Configuration, String] = dbReader
      .map(_.getLastOrderId(username))
      .flatMap(lastOrderId => dbReader.map(_.getOrderStatus(lastOrderId)))

    val lastOrderStatusReaderFor: Reader[Configuration, String] = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      lastOrderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
    } yield lastOrderStatus

    lastOrderStatusReaderFor.run(config)
  }

  /*
    Pattern
      1. you create the initial data structure
      2. you create a reader which specifies how the data structure will be manipulated later
      3. you can then map & flatmap the reader to produce derived information
      4. when you need the final piece of information, you can call run on the reader with the initial data structure
   */

  case class EmailService(emailReplyFrom: String) {
    def sendEmail(address: String, contents: String) = s"From: $emailReplyFrom; to: $address >>> $contents"
  }

  /*
    Exercise
      1. implement def emailUser
      // fetch the status of their last order
      // email them with the Email Service: "Your last order has the status: (status)"

      2. What programming patterns do Readers remind you of? Dependency Injection
   */

  val emailServiceReader: Reader[Configuration, EmailService] = Reader(config => EmailService(config.emailReplyFrom))

  def emailUser(username: String, userEmail: String): String = {
    val emailReader = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      lastOrderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
      email <- emailServiceReader.map(_.sendEmail(userEmail, s"Your last has the status: ($lastOrderStatus)"))
    } yield email

    emailReader.run(config)
  }

  def main(args: Array[String]): Unit = {
    println(getLastOrderStatus("daniel"))
    println(emailUser("john", "john@rockthejvm.com"))
  }
}
