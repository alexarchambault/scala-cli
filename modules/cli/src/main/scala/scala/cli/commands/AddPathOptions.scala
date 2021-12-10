package scala.cli.commands

import caseapp._

// format: off
final case class AddPathOptions(
  @Name("q")
    quiet: Boolean = false,
  title: String = ""
)
// format: on

object AddPathOptions {
  implicit lazy val parser: Parser[AddPathOptions] = Parser.derive
  implicit lazy val help: Help[AddPathOptions]     = Help.derive
}
