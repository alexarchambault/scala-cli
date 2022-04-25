package scala.cli.commands.publish

import coursier.cache.Cache
import coursier.util.Task

import scala.build.Logger
import scala.build.errors.BuildException
import scala.build.options.{PublishOptions => BPublishOptions}
import scala.cli.signing.shared.Secret

final case class OptionCheck(
  fieldName: String,
  directivePath: String,
  check: BPublishOptions => Boolean,
  defaultValue: () => Either[
    BuildException,
    (String, Seq[(String, String)], Seq[SetSecret])
  ]
)

object OptionCheck {
  def opt(
    fieldName: String,
    directivePath: String,
    getOpt: BPublishOptions => Option[_],
    defaultValue: () => Either[
      BuildException,
      (String, Seq[(String, String)], Seq[SetSecret])
    ]
  ): OptionCheck =
    OptionCheck(fieldName, directivePath, pubOpt => getOpt(pubOpt).nonEmpty, defaultValue)
}
