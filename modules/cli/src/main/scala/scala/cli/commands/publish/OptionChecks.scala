package scala.cli.commands.publish

import coursier.cache.Cache
import coursier.util.Task
import sttp.client3._

import java.util.Base64

import scala.build.EitherCps.{either, value}
import scala.build.Logger
import scala.cli.errors.MissingPublishOptionError
import scala.cli.signing.commands.PgpKeyId
import scala.cli.signing.shared.Secret
import sttp.model.Uri

object OptionChecks {

  private def orgCheck(options: PublishSetupOptions) = OptionCheck.opt(
    "organization",
    "publish.organization",
    _.organization,
    () =>
      options.sharedPublish.organization.map((_, Nil, Nil)).toRight {
        new MissingPublishOptionError("organization", "--organization", "publish.organization")
      }
  )

  private def nameCheck(options: PublishSetupOptions) = OptionCheck(
    "name",
    "publish.name",
    opt => opt.name.nonEmpty || opt.moduleName.nonEmpty,
    () =>
      options.sharedPublish.name.map((_, Nil, Nil)).toRight {
        new MissingPublishOptionError("name", "--name", "publish.name")
      }
  )

  private def computeVersionCheck(options: PublishSetupOptions) = OptionCheck(
    "computeVersion",
    "publish.computeVersion",
    pubOpt => pubOpt.version.nonEmpty || pubOpt.computeVersion.nonEmpty,
    () =>
      options.sharedPublish.computeVersion.map((_, Nil, Nil)).toRight {
        new MissingPublishOptionError(
          "compute version",
          "--compute-version",
          "publish.computeVersion"
        )
      }
  )

  private def repositoryCheck(options: PublishSetupOptions) = OptionCheck.opt(
    "repository",
    "publish.repository",
    _.repository,
    () =>
      options.sharedPublish.publishRepository.map((_, Nil, Nil)).toRight {
        new MissingPublishOptionError(
          "publish repository",
          "--publish-repository",
          "publish.repository"
        )
      }
  )

  private def userCheck(options: PublishSetupOptions) = OptionCheck.opt(
    "user",
    "publish.user",
    _.repoUser,
    () =>
      options.sharedPublish.user.map(_.get()).map(u =>
        ("env:PUBLISH_USER", Nil, Seq(SetSecret("PUBLISH_USER", u, force = true)))
      ).toRight {
        new MissingPublishOptionError("publish user", "--user", "publish.user")
      }
  )

  private def passwordCheck(options: PublishSetupOptions) = OptionCheck.opt(
    "password",
    "publish.password",
    _.repoPassword,
    () =>
      options.sharedPublish.password.map(_.get()).map(p =>
        ("env:PUBLISH_PASSWORD", Nil, Seq(SetSecret("PUBLISH_PASSWORD", p, force = true)))
      ).toRight {
        new MissingPublishOptionError("publish password", "--password", "publish.password")
      }
  )

  private def pgpSecretKeyCheck(
    options: PublishSetupOptions,
    coursierCache: Cache[Task],
    logger: Logger,
    backend: SttpBackend[Identity, Any]
  ) = OptionCheck.opt(
    "pgp-secret-key",
    "publish.secretKey",
    _.secretKey,
    () =>
      either {
        val (pubKeyOpt, secretKey, passwordOpt) = options.sharedPublish.secretKey match {
          case Some(secretKey) =>
            val pubKeyOpt   = options.sharedPublish.publicKey.map(_.get())
            val passwordOpt = options.sharedPublish.secretKeyPassword.map(_.get())
            (pubKeyOpt, secretKey.getBytes().map(Base64.getEncoder.encodeToString), passwordOpt)
          case None =>
            val randomSecretKey = options.randomSecretKey.getOrElse(false)
            if (randomSecretKey) {
              val password = options.sharedPublish
                .secretKeyPassword
                .map(_.get())
                .getOrElse(ThrowawayPgpSecret.pgpPassPhrase())
              val mail = value {
                options.randomSecretKeyMail
                  .toRight(
                    new MissingPublishOptionError(
                      "random secret key mail",
                      "--random-secret-key-mail",
                      ""
                    )
                  )
              }
              val (pgpPublic, pgpSecret0) = value {
                ThrowawayPgpSecret.pgpSecret(mail, password, logger, coursierCache)
              }
              val pgpSecretBase64 = pgpSecret0.map(Base64.getEncoder.encodeToString)
              (Some(pgpPublic), pgpSecretBase64, Some(password))
            }
            else
              value {
                Left(
                  new MissingPublishOptionError(
                    "publish secret key",
                    "--secret-key",
                    "publish.secretKey",
                    extraMessage =
                      ", and specify publish.sercretKeyPassword / --secret-key-password if needed. " +
                        "Alternatively, pass --random-secret-key"
                  )
                )
              }
        }
        pubKeyOpt match {
          case Some(pubKey) =>
            val keyId = value {
              (new ThrowawayPgpSecretCreateMaker).get().keyId(pubKey.value, coursierCache, logger)
            }
            val keyServers = {
              val rawKeyServers = options.sharedPgp.keyServer.filter(_.trim.nonEmpty)
              if (rawKeyServers.filter(_.trim.nonEmpty).isEmpty)
                KeyServer.allDefaults
              else
                rawKeyServers.map { keyServerUriStr =>
                  Uri.parse(keyServerUriStr) match {
                    case Left(err)  => ???
                    case Right(uri) => uri
                  }
                }
            }
            for (keyServer <- keyServers) {
              val checkResp = value {
                KeyServer.check(keyId, keyServer, backend)
                  .left.map(_ => ???)
              }
              logger.debug(s"Key server check response: $checkResp")
              val check = checkResp.isRight
              if (!check) {
                val resp = value {
                  KeyServer.add(pubKey.value, keyServer, backend)
                    .left.map(_ => ???)
                }
                logger.debug(s"Key server upload response: $resp")
                logger.message(s"Uploaded key $keyId to $keyServer")
              }
            }
          case None =>
            logger.message(
              "Warning: no public key passed, not checking if the key needs to be uploaded to a key server."
            )
        }
        val passwordSetSecret = passwordOpt.map { p =>
          SetSecret("PUBLISH_SECRET_KEY_PASSWORD", p, force = true)
        }
        val extraDirectives = passwordOpt.toSeq.map { _ =>
          "publish.secretKeyPassword" -> "env:PUBLISH_SECRET_KEY_PASSWORD"
        }
        val setSecrets =
          Seq(SetSecret("PUBLISH_SECRET_KEY", secretKey, force = true)) ++ passwordSetSecret.toSeq

        ("env:PUBLISH_SECRET_KEY", extraDirectives, setSecrets)
      }
  )

  private def licenseCheck(options: PublishSetupOptions) = OptionCheck.opt(
    "license",
    "publish.license",
    _.license,
    () =>
      options.sharedPublish.license.map((_, Nil, Nil)).toRight {
        new MissingPublishOptionError("license", "--license", "publish.license")
      }
  )

  private def urlCheck(
    options: PublishSetupOptions,
    workspace: os.Path,
    logger: Logger
  ) = OptionCheck.opt(
    "url",
    "publish.url",
    _.url,
    () => {
      def ghUrlOpt = GitRepo.ghRepoOrgName(workspace, logger) match {
        case Left(err) =>
          logger.debug(
            s"Error when trying to get GitHub repo from git to get default project URL: $err, ignoring it."
          )
          None
        case Right((org, name)) =>
          logger.message(s"Using GitHub repository $org/$name as URL")
          Some(s"https://github.com/$org/$name")
      }
      options.sharedPublish.url.orElse(ghUrlOpt).map((_, Nil, Nil)).toRight {
        new MissingPublishOptionError("url", "--url", "publish.url")
      }
    }
  )

  private def scmCheck(
    options: PublishSetupOptions,
    workspace: os.Path,
    logger: Logger
  ) = OptionCheck.opt(
    "vcs",
    "publish.versionControl",
    _.versionControl,
    () => {
      def ghVcsOpt = GitRepo.ghRepoOrgName(workspace, logger) match {
        case Left(err) =>
          logger.debug(
            s"Error when trying to get GitHub repo from git to get default project VCS: $err, ignoring it."
          )
          None
        case Right((org, name)) =>
          logger.message(s"Using GitHub repository $org/$name as VCS")
          Some(s"github:$org/$name")
      }
      options.sharedPublish.vcs.orElse(ghVcsOpt).map((_, Nil, Nil)).toRight {
        new MissingPublishOptionError("version control", "--vcs", "publish.versionControl")
      }
    }
  )

  private def developersCheck(options: PublishSetupOptions) = OptionCheck(
    "developers",
    "publish.developer",
    _.developers.nonEmpty,
    () =>
      // FIXME No headOption, add all of options.sharedPublish.developer values…
      options.sharedPublish.developer.headOption.map((_, Nil, Nil)).toRight {
        new MissingPublishOptionError("developer", "--developer", "publish.developer")
      }
  )

  def checks(
    options: PublishSetupOptions,
    workspace: os.Path,
    coursierCache: Cache[Task],
    logger: Logger,
    backend: SttpBackend[Identity, Any]
  ) = Seq(
    orgCheck(options),
    nameCheck(options),
    computeVersionCheck(options),
    repositoryCheck(options),
    userCheck(options),
    passwordCheck(options),
    pgpSecretKeyCheck(options, coursierCache, logger, backend),
    licenseCheck(options),
    urlCheck(options, workspace, logger),
    scmCheck(options, workspace, logger),
    developersCheck(options)
  )

}
