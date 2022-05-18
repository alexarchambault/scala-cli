package scala.cli.config

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._

import scala.build.errors.BuildException
import scala.cli.signing.shared.PasswordOption

sealed abstract class Entry[T] {
  def prefix: Seq[String]
  def name: String

  def parse(json: Array[Byte]): Either[Entry.EntryError, T]
  def write(value: T): Array[Byte]

  def asString(value: T): Seq[String]
  def fromString(values: Seq[String]): Either[Entry.MalformedEntry, T]

  final def fullName = (prefix :+ name).mkString(".")

  def isPasswordOption: Boolean = false
}

object Entry {

  abstract class EntryError(
    message: String,
    causeOpt: Option[Throwable] = None
  ) extends BuildException(message, cause = causeOpt.orNull)

  final class JsonReaderError(cause: JsonReaderException)
      extends EntryError("Error parsing config JSON", Some(cause))

  final class MalformedEntry(
    entry: Entry[_],
    input: Seq[String],
    messageOrExpectedShape: Either[String, String],
    cause: Option[Throwable] = None
  ) extends EntryError(
        s"Malformed values ${input.mkString(", ")} for ${entry.fullName}, " +
          messageOrExpectedShape.fold(shape => s"expected $shape", identity),
        cause
      )

  private val stringCodec: JsonValueCodec[String] = JsonCodecMaker.make

  final class StringEntry(
    val prefix: Seq[String],
    val name: String
  ) extends Entry[String] {
    def parse(json: Array[Byte]): Either[EntryError, String] =
      try Right(readFromArray(json)(stringCodec))
      catch {
        case e: JsonReaderException =>
          Left(new JsonReaderError(e))
      }
    def write(value: String): Array[Byte] =
      writeToArray(value)(stringCodec)
    def asString(value: String): Seq[String] =
      Seq(value)
    def fromString(values: Seq[String]): Either[MalformedEntry, String] =
      values match {
        case Seq(value) => Right(value)
        case _          => Left(new MalformedEntry(this, values, Left("value")))
      }
  }

  final class PasswordEntry(
    val prefix: Seq[String],
    val name: String
  ) extends Entry[PasswordOption] {
    def parse(json: Array[Byte]): Either[EntryError, PasswordOption] =
      try {
        val str = readFromArray(json)(stringCodec)
        PasswordOption.parse(str).left.map { e =>
          new MalformedEntry(this, Seq(str), Right(e))
        }
      }
      catch {
        case e: JsonReaderException =>
          Left(new JsonReaderError(e))
      }
    def write(value: PasswordOption): Array[Byte] =
      writeToArray(value.asString.value)(stringCodec)
    def asString(value: PasswordOption): Seq[String] = Seq(value.asString.value)
    def fromString(values: Seq[String]): Either[MalformedEntry, PasswordOption] =
      values match {
        case Seq(value) =>
          PasswordOption.parse(value).left.map { err =>
            new MalformedEntry(this, values, Right(err))
          }
        case _ => Left(new MalformedEntry(this, values, Left("value")))
      }

    override def isPasswordOption: Boolean = true
  }

  private val stringListCodec: JsonValueCodec[List[String]] = JsonCodecMaker.make

  final class StringListEntry(
    val prefix: Seq[String],
    val name: String
  ) extends Entry[List[String]] {
    def parse(json: Array[Byte]): Either[EntryError, List[String]] =
      try Right(readFromArray(json)(stringListCodec))
      catch {
        case e: JsonReaderException =>
          Left(new JsonReaderError(e))
      }
    def write(value: List[String]): Array[Byte] =
      writeToArray(value)(stringListCodec)
    def asString(value: List[String]): Seq[String] = value
    def fromString(values: Seq[String]): Either[MalformedEntry, List[String]] =
      Right(values.toList)
  }

}
