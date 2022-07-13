package scala.cli.commands

import caseapp._

// format: off
@HelpMessage("Fire-up a Scala REPL")
final case class ReplOptions(
  @Recurse
    shared: SharedOptions = SharedOptions(),
  @Recurse
    sharedJava: SharedJavaOptions = SharedJavaOptions(),
  @Recurse
    watch: SharedWatchOptions = SharedWatchOptions(),
  @Recurse
    compileCross: CompileCrossOptions = CompileCrossOptions(),

  @Group("Repl")
  @HelpMessage("Use Ammonite (instead of the default Scala REPL)")
  @Name("A")
  @Name("amm")
    ammonite: Option[Boolean] = None,

  @Group("Repl")
  @HelpMessage("Set the Ammonite version")
  @Name("ammoniteVer")
    ammoniteVersion: Option[String] = None,

  @Group("Repl")
  @Name("a")
  @Hidden
    ammoniteArg: List[String] = Nil,

  @Group("Repl")
  @Hidden
  @HelpMessage("Don't actually run the REPL, just fetch it")
    replDryRun: Boolean = false,

  @Group("Repl")
  @Hidden
  @HelpMessage("Run Spark REPL, using a vanilla Spark distribution downloaded by Scala CLI")
    spark: Boolean = false,

  @Group("Repl")
  @Hidden
  @HelpMessage("Run Spark REPL")
  @ExtraName("sparkStandalone")
    standaloneSpark: Boolean = false,

  @Group("Repl")
  @HelpMessage("Preload file")
  @ExtraName("preload")
  @ExtraName("I") // spark-shell accepts that
    predef: Option[String] = None
)
// format: on

object ReplOptions {
  implicit lazy val parser: Parser[ReplOptions] = Parser.derive
  implicit lazy val help: Help[ReplOptions]     = Help.derive
}
