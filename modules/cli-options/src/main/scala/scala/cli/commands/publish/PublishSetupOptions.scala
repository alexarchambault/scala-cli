package scala.cli.commands.publish

import caseapp._

import scala.cli.commands.pgp.SharedPgpPushPullOptions
import scala.cli.commands.{
  CoursierOptions,
  LoggingOptions,
  SharedDirectoriesOptions,
  SharedInputOptions,
  SharedWorkspaceOptions
}
import scala.cli.signing.shared.PasswordOption
import scala.cli.signing.util.ArgParsers._

// format: off
final case class PublishSetupOptions(
  @Recurse
    logging: LoggingOptions = LoggingOptions(),
  @Recurse
    directories: SharedDirectoriesOptions = SharedDirectoriesOptions(),
  @Recurse
    coursier: CoursierOptions = CoursierOptions(),
  @Recurse
    workspace: SharedWorkspaceOptions = SharedWorkspaceOptions(),
  @Recurse
    input: SharedInputOptions = SharedInputOptions(),
  @Recurse
    sharedPublish: SharedPublishOptions = SharedPublishOptions(),
  @Recurse
    sharedPgp: SharedPgpPushPullOptions = SharedPgpPushPullOptions(),

  check: Boolean = false,
  token: Option[PasswordOption] = None,
  randomSecretKey: Option[Boolean] = None,
  randomSecretKeyMail: Option[String] = None
)
// format: on

object PublishSetupOptions {
  implicit lazy val parser: Parser[PublishSetupOptions] = Parser.derive
  implicit lazy val help: Help[PublishSetupOptions]     = Help.derive
}
