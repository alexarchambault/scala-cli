package scala.build.options

import scala.build.options.publish.{ComputeVersion, Signer}
import scala.cli.signing.shared.PasswordOption

final case class PublishContextualOptions(
  repository: Option[String] = None,
  repositoryIsIvy2LocalLike: Option[Boolean] = None,
  sourceJar: Option[Boolean] = None,
  docJar: Option[Boolean] = None,
  gpgSignatureId: Option[String] = None,
  gpgOptions: List[String] = Nil,
  signer: Option[Signer] = None,
  secretKey: Option[PasswordOption] = None,
  secretKeyPassword: Option[PasswordOption] = None,
  repoUser: Option[PasswordOption] = None,
  repoPassword: Option[PasswordOption] = None,
  computeVersion: Option[ComputeVersion] = None,
  checksums: Option[Seq[String]] = None
)

object PublishContextualOptions {
  implicit val monoid: ConfigMonoid[PublishContextualOptions] = ConfigMonoid.derive
}
