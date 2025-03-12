package part3datamanipulation

import cats.kernel.Semigroup

import scala.annotation.tailrec
import scala.util.Try

object DataManipulation {

  import cats.data.Validated
  val aValidValue: Validated[String, Int] = Validated.valid(42) // "right" value
  val anInvalidValue: Validated[String, Int]= Validated.invalid("Something went wrong") // "left" value

  val aTest: Validated[String, Int] = Validated.cond(42 > 39, 99, "meaning of life is greater")

  def testPrime_v0(n: Int) = (2 until n).forall(n % _ != 0)

  def testPrime(n: Int): Boolean = {
    @tailrec
    def tailrecPrime(d: Int): Boolean =
      if (d <= 1) true
      else n % d != 0 && tailrecPrime(d - 1)

    if (n == 0 || n == 1 || n == -1) false
    else tailrecPrime(Math.abs(n/2))
  }
  /*
    Exercise
      1. implement testNumber with Either[List[String], Int]
        - n must be prime
        - n must be non-negative
        - n <= 100
        - n must be even
   */
  def testNumber(n: Int): Either[List[String], Int] = {
    val isNotEven: List[String] = if (n % 2 == 0) List() else List("Number must be even")
    val isNegative: List[String] = if (n >= 0) List() else List("Number must be non-negative")
    val isTooBig: List[String] = if (n <= 100) List() else List("Number must be less than or equal to 100")
    val isNotPrime: List[String] = if (testPrime(n)) List() else List("Number must be a prime")

    if (n % 2 == 0 && n >= 0 && n <= 100 && testPrime(n)) Right(n)
    else Left(isNotEven ++ isNegative ++ isTooBig ++ isNotPrime)
  }

  def validateNumber(n: Int): Validated[List[String], Int] = {
    implicit val combineIntMax: Semigroup[Int] = Semigroup.instance[Int](Math.max)

    Validated.cond(n % 2 == 0, n, List("Number must be even"))
      .combine(Validated.cond(n > 0, n, List("Number must be non-negative")))
      .combine(Validated.cond(n <= 100, n, List("Number must be less than or equal to 100")))
      .combine(Validated.cond(testPrime(n), n, List("Number must be a prime")))
  }

  // chain
  aValidValue.andThen(_ => anInvalidValue)
  //test a valid value
  aValidValue.ensure(List("something went wrong"))(_ % 2 ==0)
  // transform
  aValidValue.map(_ + 1)
  aValidValue.leftMap(_.length)
  aValidValue.bimap(_.length, _ + 1)

  // interop with stdlib

  val eitherToValidated: Validated[List[String], Int] = Validated.fromEither(Right(42))
  val optionToValidated: Validated[List[String], Int] = Validated.fromOption(None, List("nothing present here"))
  val tryToValidated: Validated[Throwable, Int] = Validated.fromTry(Try("something".toInt))

  // backwards
  aValidValue.toOption
  aValidValue.toEither

  /*
    Exercise
      2. form validation

      fields are
        - name
        - email
        - password

      rules are
        - name, email and password MUST be specified
        - name must not be blank
        - email must have "@"
        - password must have >= 10 characters
   */

  object FormValidation {
    type FormValidation[T] = Validated[List[String], T]

    def getValue(form: Map[String, String], fieldName: String): FormValidation[String] =
      Validated.fromOption(form.get(fieldName), List(s"The field $fieldName must be specified"))

    def notBlank(value: String, fieldName: String): FormValidation[String] =
      Validated.cond(value.nonEmpty, value, List(s"The field $fieldName must not be blank"))

    def emailProperForm(email: String): FormValidation[String] =
      Validated.cond(email.contains("@"), email, List("Email is invalid"))

    def passwordCheck(password: String): FormValidation[String] =
      Validated.cond(password.length > 10, password, List("Passwords must be at least 10 characters long"))

    def validateForm(form: Map[String, String]): FormValidation[String] =
      getValue(form, "name").andThen(name => notBlank(name, "name"))
        .combine(getValue(form, "email").andThen(emailProperForm))
        .combine(getValue(form, "password").andThen(passwordCheck))
        .map(_ => "Success")
  }

  import cats.syntax.validated._

  val aValidMeaningOfLife: Validated[List[String], Int] = 42.valid[List[String]]
  val anError: Validated[String, Int] = "somthing went wrong".invalid[Int]

  def main(args: Array[String]): Unit = {
    println(validateNumber(2))
    println(FormValidation.validateForm(Map(
      "name" -> "daniel",
      "email" -> "email@example.com",
      "password" -> "12345678910"
    )))
    println(aValidMeaningOfLife)
    println(anError)
  }
}
