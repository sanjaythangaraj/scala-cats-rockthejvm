package part5alien

import cats.Monoid

object Invariants {

  trait Crypto[A] { self =>
    def encrypt(value: A): String
    def decrypt(encrypted: String): A

    def imap[B](back: B => A, forth: A => B): Crypto[B] = new Crypto[B] {
      override def encrypt(value: B): String = self.encrypt(back(value))
      override def decrypt(encrypted: String): B = forth(self.decrypt(encrypted))
    }
  }

  def encrypt[A](value: A)(implicit crypto: Crypto[A]): String = crypto.encrypt(value)
  def decrypt[A](encrypted: String)(implicit crypto: Crypto[A]): A = crypto.decrypt(encrypted)

  implicit val caesarCypher: Crypto[String] = new Crypto[String] {
    override def encrypt(value: String): String = value.map(c => (c+2).toChar)
    override def decrypt(encrypted: String): String = encrypted.map(c => (c - 2).toChar)
  }

  // How can we support ints, doubles, Option[String]?

  implicit val doubleCrypto: Crypto[Double] = caesarCypher.imap(_.toString, _.toDouble)

  implicit val optionStringCrypto: Crypto[Option[String]] = caesarCypher.imap(_.getOrElse(""), Option(_))

  implicit def optionCrypto[T](implicit crypto: Crypto[T], monoid: Monoid[T]): Crypto[Option[T]] =
    crypto.imap(_.getOrElse(monoid.empty), Option(_))

  import cats.Invariant
  import cats.Show

  val showOptionString: Show[Option[String]] = Invariant[Show].imap(Show[String])(Option(_))(_.getOrElse(""))

  import cats.syntax.invariant._

  val showOptionString2: Show[Option[String]] = Show[String].imap(Option(_))(_.getOrElse("")) // identical

  trait MyInvaraint[W[_]] {
    def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B]
  }

  trait MyContravaraint[W[_]] extends MyInvaraint[W] {
    def contramap[A, B](wa: W[A])(back: B => A): W[B]

    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] =
      contramap(wa)(back)
  }

  trait MyFunctor[W[_]] extends MyInvaraint[W] {
    def map[A, B](wa: W[A])(forth: A => B): W[B]

    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] =
      map(wa)(forth)
  }

  def main(args: Array[String]): Unit = {
    val encrypted: String = encrypt("Let's encrypt")
    val decrypted: String = decrypt[String](encrypted)

    println(encrypted)
    println(decrypted)

    println(encrypt(Math.PI))
    println(decrypt[Double](encrypt(Math.PI)))

    println(encrypt(Option("Let's encrypt")))
    println(decrypt[Option[String]](encrypted))

    println(encrypt(Option(Math.PI)))
    println(decrypt[Option[Double]](encrypt(Math.PI)))
  }

}
