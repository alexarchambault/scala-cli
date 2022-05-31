package scala.build.preprocessing.directives

import scala.build.EitherCps.{either, value}
import scala.build.Logger
import scala.build.Ops._
import scala.build.errors.{
  BuildException,
  CompositeBuildException,
  MalformedInputError,
  UnexpectedDirectiveError
}
import scala.build.options.publish.{ComputeVersion, Developer, License, Vcs}
import scala.build.options.{
  BuildOptions,
  PostBuildOptions,
  PublishContextualOptions,
  PublishOptions
}
import scala.cli.signing.shared.PasswordOption

case object UsingPublishContextualDirectiveHandler extends UsingDirectiveHandler {

  private def prefix = "publish."

  def name        = "Publish"
  def description = "Set parameters for publishing (contextual)"
  def usage       = s"//> using $prefix(organization|name|version) [value]"

  override def usageMd =
    s"""`//> using ${prefix}organization `"value"
       |`//> using ${prefix}name `"value"
       |`//> using ${prefix}version `"value"
       |""".stripMargin

  private def q = "\""
  override def examples = Seq(
    s"//> using ${prefix}organization ${q}io.github.myself$q",
    s"//> using ${prefix}name ${q}my-library$q",
    s"//> using ${prefix}version ${q}0.1.1$q"
  )
  def keys = Seq(
    "computeVersion",
    "compute-version",
    "repository",
    "gpgKey",
    "gpg-key",
    "gpgOption",
    "gpg-option",
    "gpgOptions",
    "gpg-options",
    "secretKey",
    "secretKeyPassword",
    "user",
    "password"
  ).map(prefix + _)

  override def getValueNumberBounds(key: String) = key match {
    case "gpgOptions" | "gpg-options" | "gpgOption" | "gpg-option" =>
      UsingDirectiveValueNumberBounds(1, Int.MaxValue)
    case _ => UsingDirectiveValueNumberBounds(1, 1)
  }

  def handleValues(
    scopedDirective: ScopedDirective,
    logger: Logger
  ): Either[BuildException, ProcessedUsingDirective] = either {

    val groupedScopedValuesContainer = value(checkIfValuesAreExpected(scopedDirective))

    val severalValues = groupedScopedValuesContainer.scopedStringValues.map(_.positioned)
    val singleValue   = severalValues.head

    val strippedKey =
      if (scopedDirective.directive.key.startsWith(prefix))
        scopedDirective.directive.key.stripPrefix(prefix)
      else
        value(Left(new UnexpectedDirectiveError(scopedDirective.directive.key)))

    val publishOptions = strippedKey match {
      case "computeVersion" | "compute-version" =>
        value {
          ComputeVersion.parse(singleValue).map {
            computeVersion =>
              PublishOptions(
                contextual = PublishContextualOptions(
                  computeVersion = Some(
                    computeVersion
                  )
                )
              )
          }
        }
      case "repository" =>
        PublishOptions(contextual = PublishContextualOptions(repository = Some(singleValue.value)))
      case "gpgKey" | "gpg-key" =>
        PublishOptions(contextual =
          PublishContextualOptions(gpgSignatureId = Some(singleValue.value))
        )
      case "gpgOptions" | "gpg-options" | "gpgOption" | "gpg-option" =>
        PublishOptions(contextual =
          PublishContextualOptions(gpgOptions = severalValues.map(_.value).toList)
        )
      case "secretKey" =>
        PublishOptions(contextual =
          PublishContextualOptions(secretKey = Some(value(parsePasswordOption(singleValue.value))))
        )
      case "secretKeyPassword" =>
        PublishOptions(contextual =
          PublishContextualOptions(secretKeyPassword =
            Some(value(parsePasswordOption(singleValue.value)))
          )
        )
      case "user" =>
        PublishOptions(contextual =
          PublishContextualOptions(repoUser = Some(value(parsePasswordOption(singleValue.value))))
        )
      case "password" =>
        PublishOptions(contextual =
          PublishContextualOptions(repoPassword =
            Some(value(parsePasswordOption(singleValue.value)))
          )
        )
      case _ =>
        value(Left(new UnexpectedDirectiveError(scopedDirective.directive.key)))
    }

    val options = BuildOptions(
      notForBloopOptions = PostBuildOptions(
        publishOptions = publishOptions
      )
    )

    ProcessedDirective(Some(options), Seq.empty)
  }

  private def parsePasswordOption(input: String): Either[BuildException, PasswordOption] =
    PasswordOption.parse(input)
      .left.map(_ =>
        new MalformedInputError("secret", input, "file:_path_|value:_value_|env:_env_var_name_")
      )
}
