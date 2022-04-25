package scala.cli.commands.publish

import caseapp.core.RemainingArgs
import coursier.cache.Cache
import coursier.util.Task

import java.nio.charset.StandardCharsets

import scala.build.Logger
import scala.build.errors.BuildException
import scala.cli.signing.commands.{PgpCreate, PgpCreateOptions, PgpKeyId}
import scala.cli.signing.shared.{PasswordOption, Secret}

class ThrowawayPgpSecretCreateJvm extends ThrowawayPgpSecretCreate {
  override def call(
    pubKey: String,
    secKey: String,
    mail: String,
    quiet: Boolean,
    password: String,
    cache: Cache[Task],
    logger: Logger
  ): Either[BuildException, Int] = {

    PgpCreate.tryRun(
      PgpCreateOptions(
        email = mail,
        password = PasswordOption.Value(Secret(password)),
        pubDest = Some(pubKey),
        secretDest = Some(secKey),
        quiet = quiet
      ),
      RemainingArgs(Seq(), Nil)
    )
    Right(0)
  }

  override def keyId(
    key: String,
    cache: Cache[Task],
    logger: Logger
  ): Either[BuildException, String] =
    PgpKeyId.get(key.getBytes(StandardCharsets.UTF_8), fingerprint = false)
      .headOption
      .toRight(???)
}
