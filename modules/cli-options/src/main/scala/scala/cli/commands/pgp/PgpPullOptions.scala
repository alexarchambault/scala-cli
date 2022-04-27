package scala.cli.commands.pgp

import caseapp._

import scala.cli.commands.LoggingOptions

// format: off
final case class PgpPullOptions(
  @Recurse
    logging: LoggingOptions = LoggingOptions(),
  @Recurse
    shared: SharedPgpPushPullOptions = SharedPgpPushPullOptions(),
  allowEmpty: Boolean = false
)
// format: on

object PgpPullOptions {
  implicit lazy val parser: Parser[PgpPullOptions] = Parser.derive
  implicit lazy val help: Help[PgpPullOptions]     = Help.derive
}
