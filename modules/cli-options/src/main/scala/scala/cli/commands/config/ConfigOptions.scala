package scala.cli.commands.config

import caseapp._

import scala.cli.commands.{CoursierOptions, LoggingOptions, SharedDirectoriesOptions}

// format: off
final case class ConfigOptions(
  @Recurse
    logging: LoggingOptions = LoggingOptions(),
  @Recurse
    directories: SharedDirectoriesOptions = SharedDirectoriesOptions(),
  @Recurse
    coursier: CoursierOptions = CoursierOptions(),
  @Hidden
    dump: Boolean = false,
  createKey: Boolean = false,
  password: Boolean = false
)
// format: on

object ConfigOptions {
  implicit lazy val parser: Parser[ConfigOptions] = Parser.derive
  implicit lazy val help: Help[ConfigOptions]     = Help.derive
}
