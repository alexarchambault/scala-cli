package scala.cli.commands.publish

import scala.build.errors.BuildException
import scala.build.options.{PublishOptions => BPublishOptions}

final case class OptionCheck(
  fieldName: String,
  directivePath: String,
  check: BPublishOptions => Boolean,
  defaultValue: () => Either[BuildException, OptionCheck.DefaultValue],
  kind: OptionCheck.Kind
)

object OptionCheck {

  final case class DefaultValue(
    getValue: () => Either[BuildException, String],
    extraDirectives: Seq[(String, String)],
    ghSecrets: Seq[SetSecret]
  )

  object DefaultValue {
    def simple(
      value: String,
      extraDirectives: Seq[(String, String)],
      ghSecrets: Seq[SetSecret]
    ): DefaultValue =
      DefaultValue(
        () => Right(value),
        extraDirectives,
        ghSecrets
      )
  }

  sealed abstract class Kind extends Product with Serializable

  object Kind {
    case object Core       extends Kind
    case object Extra      extends Kind
    case object Repository extends Kind
    case object Signing    extends Kind

    val all = Seq(Core, Extra, Repository, Signing)

    def parse(input: String): Option[Kind] =
      input match {
        case "core"                => Some(Core)
        case "extra"               => Some(Extra)
        case "repo" | "repository" => Some(Repository)
        case "signing"             => Some(Signing)
        case _                     => None
      }
    def parseList(input: String): Either[Seq[String], Seq[Kind]] = {
      val results = input.split(",").map(v => (v, parse(v))).toSeq
      val unrecognized = results.collect {
        case (v, None) => v
      }
      if (unrecognized.isEmpty)
        Right {
          results.collect {
            case (_, Some(kind)) => kind
          }
        }
      else
        Left(unrecognized)
    }
  }

  def opt(
    fieldName: String,
    directivePath: String,
    getOpt: BPublishOptions => Option[_],
    defaultValue: () => Either[BuildException, DefaultValue],
    kind: Kind
  ): OptionCheck =
    OptionCheck(
      fieldName,
      directivePath,
      pubOpt => getOpt(pubOpt).nonEmpty,
      defaultValue,
      kind
    )
}
