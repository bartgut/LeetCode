package ru.chocholo

import scala.collection.immutable.HashSet


case class EmailAddress(localName: String, domainName: String)

object EmailAddress {
  def make(raw: String): Either[String, EmailAddress] = {
    val splitted = raw.split('@')
    if (splitted.length == 2) Right(EmailAddress(splitted(0), splitted(1)))
    else Left("Missing @ in the input")
  }

  implicit class EmailAddressExt(emailAddress: EmailAddress) {
    def normalize: EmailAddress =
      emailAddress.copy(emailAddress.localName.filter(_ != '.').takeWhile(_ != '+'),
        emailAddress.domainName)

    def toRawString: String =
      emailAddress.localName + "@" + emailAddress.domainName
  }

}

object UniqueEmailAddresses {

  def numUniqueEmails(emails: Array[String]): Int = {
    emails.map(EmailAddress.make(_).map(_.normalize))
      .collect { case Right(email) => email }
      .foldLeft(HashSet.empty[String]) { (set, address) =>
        set.incl(address.toRawString)
      }.size
  }

  def main(args: Array[String]): Unit = {

  }
}
