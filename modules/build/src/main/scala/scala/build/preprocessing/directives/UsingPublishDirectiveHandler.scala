package scala.build.preprocessing.directives

import scala.build.EitherCps.{either, value}
import scala.build.Logger
import scala.build.errors.{BuildException, UnexpectedDirectiveError}
import scala.build.options.{BuildOptions, PostBuildOptions, PublishOptions}
import scala.build.preprocessing.ScopePath

case object UsingPublishDirectiveHandler extends UsingDirectiveHandler {

  private def prefix = "publish."

  def name        = "Publish"
  def description = "Set parameters for publishing"
  def usage       = s"//> using $prefix(organization|moduleName|version) [value]"

  override def usageMd =
    s"""`//> using ${prefix}organization `"value"
       |`//> using ${prefix}moduleName `"value"
       |`//> using ${prefix}version `"value"
       |""".stripMargin

  private def q = "\""
  override def examples = Seq(
    s"//> using ${prefix}organization ${q}io.github.myself$q",
    s"//> using ${prefix}moduleName ${q}my-library$q",
    s"//> using ${prefix}version ${q}0.1.1$q"
  )
  def keys = Seq(
    "organization",
    "name",
    "version",
    "url",
    "license",
    "versionControl",
    "version-control",
    "scm",
    "description",
    "developer",
    "scalaVersionSuffix",
    "scala-version-suffix",
    "scalaPlatformSuffix",
    "scala-platform-suffix",
    "repository",
    "gpgKey",
    "gpg-key"
  ).map(prefix + _)

  def handleValues(
    directive: StrictDirective,
    path: Either[String, os.Path],
    cwd: ScopePath,
    logger: Logger
  ): Either[BuildException, ProcessedUsingDirective] = either {
    val value0 = value(DirectiveUtil.singleStringValue(directive, path, cwd))

    if (!directive.key.startsWith(prefix))
      value(Left(new UnexpectedDirectiveError(directive)))

    val publishOptions = directive.key.stripPrefix(prefix) match {
      case "organization" =>
        PublishOptions(organization = Some(value0))
      case "name" =>
        PublishOptions(name = Some(value0))
      case "version" =>
        PublishOptions(name = Some(value0))
      case "url" =>
        PublishOptions(url = Some(value0))
      case "license" =>
        PublishOptions(license = Some(value0.map(PublishOptions.parseLicense(_))))
      case "versionControl" | "version-control" | "scm" =>
        PublishOptions(versionControl = Some(value(PublishOptions.parseVcs(value0))))
      case "description" =>
        PublishOptions(description = Some(value0.value))
      case "developer" =>
        PublishOptions(developers = Seq(value(PublishOptions.parseDeveloper(value0))))
      case "scalaVersionSuffix" | "scala-version-suffix" =>
        PublishOptions(scalaVersionSuffix = Some(value0.value))
      case "scalaPlatformSuffix" | "scala-platform-suffix" =>
        PublishOptions(scalaPlatformSuffix = Some(value0.value))
      case "repository" =>
        PublishOptions(repository = Some(value0.value))
      case "gpgKey" | "gpg-key" =>
        PublishOptions(gpgSignatureId = Some(value0.value))
      case _ =>
        value(Left(new UnexpectedDirectiveError(directive)))
    }
    val options = BuildOptions(
      notForBloopOptions = PostBuildOptions(
        publishOptions = publishOptions
      )
    )
    ProcessedDirective(Some(options), Seq.empty)
  }
}
