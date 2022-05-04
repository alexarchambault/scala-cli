package scala.cli.commands.pgp

import caseapp._

import scala.cli.commands.LoggingOptions

// format: off
final case class SharedPgpPushPullOptions(
  keyServer: List[String] = Nil
)
// format: on

object SharedPgpPushPullOptions {
  lazy val parser: Parser[SharedPgpPushPullOptions]                           = Parser.derive
  implicit lazy val parserAux: Parser.Aux[SharedPgpPushPullOptions, parser.D] = parser
  implicit lazy val help: Help[SharedPgpPushPullOptions]                      = Help.derive
}
