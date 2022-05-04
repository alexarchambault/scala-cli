package scala.cli.commands.pgp

import caseapp._

import scala.cli.commands.LoggingOptions

// format: off
final case class PgpPushOptions(
  @Recurse
    logging: LoggingOptions = LoggingOptions(),
  @Recurse
    shared: SharedPgpPushPullOptions = SharedPgpPushPullOptions(),
  @ExtraName("f")
    force: Boolean = false,
  allowEmpty: Boolean = false
)
// format: on

object PgpPushOptions {
  implicit lazy val parser: Parser[PgpPushOptions] = Parser.derive
  implicit lazy val help: Help[PgpPushOptions]     = Help.derive
}
