package scala.cli.commands.publish

import coursier.cache.Cache
import coursier.util.Task
import sttp.client3._
import sttp.model.Uri

import java.util.Base64

import scala.build.EitherCps.{either, value}
import scala.build.Logger
import scala.build.Ops._
import scala.build.errors.{BuildException, CompositeBuildException}
import scala.cli.commands.config.ThrowawayPgpSecret
import scala.cli.commands.pgp.{KeyServer, PgpProxyMaker}
import scala.cli.config.{ConfigDb, Entries}
import scala.cli.errors.MissingPublishOptionError

object OptionChecks {

  private def orgCheck(
    options: PublishSetupOptions,
    workspace: os.Path,
    logger: Logger
  ) = OptionCheck.opt(
    "organization",
    "publish.organization",
    _.organization,
    () => {

      def viaGitHubRemoteOpt = GitRepo.ghRepoOrgName(workspace, logger) match {
        case Left(err) =>
          logger.debug(
            s"Error when trying to get GitHub repo from git to compute default organization: $err, ignoring it."
          )
          None
        case Right((org, _)) =>
          logger.message(s"Computing default organization from GitHub account $org")
          Some(s"io.github.$org")
      }

      val orgOpt = options.publishParams.organization
        .orElse(viaGitHubRemoteOpt)

      orgOpt.map(OptionCheck.DefaultValue.simple(_, Nil, Nil)).toRight {
        new MissingPublishOptionError(
          "organization",
          "--organization",
          "publish.organization"
        )
      }
    },
    OptionCheck.Kind.Core
  )

  private def nameCheck(
    options: PublishSetupOptions,
    workspace: os.Path,
    logger: Logger
  ) = OptionCheck(
    "name",
    "publish.name",
    opt => opt.name.nonEmpty || opt.moduleName.nonEmpty,
    () => {
      def fromWorkspaceDirName = {
        val n = workspace.last
        logger.message(s"Using workspace directory name $n as default name for publishing")
        n
      }
      val name = options.publishParams.name.getOrElse(fromWorkspaceDirName)
      Right(OptionCheck.DefaultValue.simple(name, Nil, Nil))
    },
    OptionCheck.Kind.Core
  )

  private def computeVersionCheck(
    options: PublishSetupOptions,
    workspace: os.Path,
    logger: Logger
  ) = OptionCheck(
    "computeVersion",
    "publish" + (if (options.publishParams.isCi) ".ci" else "") + ".computeVersion",
    pubOpt =>
      pubOpt.version.nonEmpty || pubOpt.retained(
        options.publishParams.isCi
      ).computeVersion.nonEmpty,
    () => {
      def fromGitOpt =
        if (GitRepo.isGitRepo(workspace)) {
          logger.message(
            "Workspace is a git repository, assuming versions are computed from git tags"
          )
          Some("git:tag")
        }
        else
          None
      val cv = options.publishParams.computeVersion
        .orElse(fromGitOpt)
      cv.map(OptionCheck.DefaultValue.simple(_, Nil, Nil)).toRight {
        new MissingPublishOptionError(
          "compute version",
          "--compute-version",
          "publish.computeVersion"
        )
      }
    },
    OptionCheck.Kind.Core
  )

  private def repositoryCheck(
    options: PublishSetupOptions,
    logger: Logger
  ) = OptionCheck.opt(
    "repository",
    "publish" + (if (options.publishParams.isCi) ".ci" else "") + ".repository",
    _.retained(options.publishParams.isCi).repository,
    () => {
      val repo = options.publishRepo.publishRepository.getOrElse {
        logger.message(
          "No publish repository specified, assuming Maven Central via its s01 server."
        )
        "central-s01"
      }
      Right(OptionCheck.DefaultValue.simple(repo, Nil, Nil))
    },
    OptionCheck.Kind.Repository
  )

  private def userCheck(
    options: PublishSetupOptions,
    configDb: => ConfigDb,
    logger: Logger
  ) = OptionCheck.opt(
    "user",
    "publish" + (if (options.publishParams.isCi) ".ci" else "") + ".user",
    _.retained(options.publishParams.isCi).repoUser,
    () =>
      either {
        val user0 = options.publishRepo.user match {
          case Some(value0) => value0
          case None =>
            val userOpt = value(configDb.get(Entries.sonatypeUser))
            userOpt match {
              case Some(user) =>
                logger.message("Using Sonatype user from Scala CLI configuration")
                user
              case None =>
                value {
                  Left {
                    new MissingPublishOptionError(
                      "publish user",
                      "--user",
                      "publish.user",
                      configKeys = Seq(Entries.sonatypeUser.fullName)
                    )
                  }
                }
            }
        }

        if (options.publishParams.isCi)
          OptionCheck.DefaultValue.simple(
            "env:PUBLISH_USER",
            Nil,
            Seq(SetSecret("PUBLISH_USER", user0.get(), force = true))
          )
        else
          OptionCheck.DefaultValue.simple(
            user0.asString.value,
            Nil,
            Nil
          )
      },
    OptionCheck.Kind.Repository
  )

  private def passwordCheck(
    options: PublishSetupOptions,
    configDb: => ConfigDb,
    logger: Logger
  ) = OptionCheck.opt(
    "password",
    "publish" + (if (options.publishParams.isCi) ".ci" else "") + ".password",
    _.retained(options.publishParams.isCi).repoPassword,
    () =>
      either {
        val password = options.publishRepo.password match {
          case Some(password0) => password0
          case None =>
            val passwordOpt = value(configDb.get(Entries.sonatypePassword))
            passwordOpt match {
              case Some(password0) =>
                logger.message("Using Sonatype password from Scala CLI configuration")
                password0
              case None =>
                value {
                  Left {
                    new MissingPublishOptionError(
                      "publish password",
                      "--password",
                      "publish.password",
                      configKeys = Seq(Entries.sonatypePassword.fullName)
                    )
                  }
                }
            }
        }

        if (options.publishParams.isCi)
          OptionCheck.DefaultValue.simple(
            "env:PUBLISH_PASSWORD",
            Nil,
            Seq(SetSecret("PUBLISH_PASSWORD", password.get(), force = true))
          )
        else
          OptionCheck.DefaultValue.simple(
            password.asString.value,
            Nil,
            Nil
          )
      },
    OptionCheck.Kind.Repository
  )

  private def pgpSecretKeyCheck(
    options: PublishSetupOptions,
    coursierCache: Cache[Task],
    configDb: ConfigDb,
    logger: Logger,
    backend: SttpBackend[Identity, Any]
  ) = OptionCheck(
    "pgp-secret-key",
    "publish" + (if (options.publishParams.isCi) ".ci" else "") + ".secretKey",
    opt => {
      val opt0 = opt.retained(options.publishParams.isCi)
      !opt0.repository.orElse(options.publishRepo.publishRepository).contains("github") &&
      opt0.secretKey.nonEmpty
    },
    () =>
      either {
        val (pubKeyOpt, secretKey, passwordOpt) = options.publishParams.secretKey match {
          case Some(secretKey) =>
            val pubKeyOpt   = options.publishParams.publicKey.map(_.get())
            val passwordOpt = options.publishParams.secretKeyPassword.map(_.get())
            (pubKeyOpt, secretKey.getBytes().map(Base64.getEncoder.encodeToString), passwordOpt)
          case None =>
            value(configDb.get(Entries.pgpSecretKey)) match {
              case Some(secretKey) =>
                val pubKeyOpt   = value(configDb.get(Entries.pgpPublicKey))
                val passwordOpt = value(configDb.get(Entries.pgpSecretKeyPassword))
                (
                  pubKeyOpt.map(_.get()),
                  secretKey.getBytes().map(Base64.getEncoder.encodeToString),
                  passwordOpt.map(_.get())
                )
              case None =>
                val randomSecretKey = options.randomSecretKey.getOrElse(false)
                if (randomSecretKey) {
                  val password = options.publishParams
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
                        configKeys = Seq(Entries.pgpSecretKey.fullName),
                        extraMessage =
                          ", and specify publish.secretKeyPassword / --secret-key-password if needed. " +
                            "Alternatively, pass --random-secret-key"
                      )
                    )
                  }
            }
        }
        val pushKey: () => Either[BuildException, Unit] = pubKeyOpt match {
          case Some(pubKey) =>
            val keyId = value {
              (new PgpProxyMaker).get().keyId(
                pubKey.value,
                "[generated key]",
                coursierCache,
                logger
              )
            }
            val keyServers = {
              val rawKeyServers = options.sharedPgp.keyServer.filter(_.trim.nonEmpty)
              if (rawKeyServers.filter(_.trim.nonEmpty).isEmpty)
                KeyServer.allDefaults
              else
                rawKeyServers.map { keyServerUriStr =>
                  Uri.parse(keyServerUriStr) match {
                    case Left(_)    => ???
                    case Right(uri) => uri
                  }
                }
            }
            () =>
              keyServers
                .map { keyServer =>
                  val e: Either[BuildException, Unit] = either {
                    val checkResp = value {
                      KeyServer.check(keyId, keyServer, backend)
                        .left.map(_ => ??? : BuildException)
                    }
                    logger.debug(s"Key server check response: $checkResp")
                    val check = checkResp.isRight
                    if (!check) {
                      val resp = value {
                        KeyServer.add(pubKey.value, keyServer, backend)
                          .left.map(_ => ??? : BuildException)
                      }
                      logger.debug(s"Key server upload response: $resp")
                      logger.message(s"Uploaded key $keyId to $keyServer")
                    }
                  }
                  e
                }
                .sequence
                .left.map(CompositeBuildException(_))
                .map(_ => ())
          case None =>
            logger.message(
              "Warning: no public key passed, not checking if the key needs to be uploaded to a key server."
            )
            () => Right(())
        }
        val (passwordSetSecret, extraDirectives) = passwordOpt
          .map { p =>
            val dir =
              if (options.publishParams.isCi)
                "publish.secretKeyPassword" -> "env:PUBLISH_SECRET_KEY_PASSWORD"
              else
                "publish.secretKeyPassword" -> "env:PUBLISH_SECRET_KEY_PASSWORD"
            val setSec = SetSecret("PUBLISH_SECRET_KEY_PASSWORD", p, force = true)
            (Seq(setSec), Seq(dir))
          }
          .getOrElse((Nil, Nil))

        if (options.publishParams.isCi) {
          val setSecrets =
            Seq(SetSecret("PUBLISH_SECRET_KEY", secretKey, force = true)) ++ passwordSetSecret
          OptionCheck.DefaultValue(
            () => pushKey().map(_ => "env:PUBLISH_SECRET_KEY"),
            extraDirectives,
            setSecrets
          )
        }
        else
          OptionCheck.DefaultValue(
            () => ???,
            extraDirectives,
            Nil
          )
      },
    OptionCheck.Kind.Signing
  )

  private def defaultLicense = "Apache-2.0"
  private def licenseCheck(
    options: PublishSetupOptions,
    logger: Logger
  ) = OptionCheck.opt(
    "license",
    "publish.license",
    _.license,
    () => {
      val license = options.publishParams.license.getOrElse {
        logger.message(s"Using default license $defaultLicense")
        defaultLicense
      }
      Right(OptionCheck.DefaultValue.simple(license, Nil, Nil))
    },
    OptionCheck.Kind.Extra
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
      options.publishParams.url.orElse(ghUrlOpt).map(
        OptionCheck.DefaultValue.simple(_, Nil, Nil)
      ).toRight {
        new MissingPublishOptionError("url", "--url", "publish.url")
      }
    },
    OptionCheck.Kind.Extra
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
      options.publishParams.vcs.orElse(ghVcsOpt).map(
        OptionCheck.DefaultValue.simple(_, Nil, Nil)
      ).toRight {
        new MissingPublishOptionError("version control", "--vcs", "publish.versionControl")
      }
    },
    OptionCheck.Kind.Extra
  )

  private def developersCheck(
    options: PublishSetupOptions,
    configDb: => ConfigDb,
    logger: Logger
  ) = OptionCheck(
    "developers",
    "publish.developer",
    _.developers.nonEmpty,
    () =>
      either {
        // FIXME No headOption, add all of options.publishParams.developer values…
        val strValue = options.publishParams.developer.headOption match {
          case None =>
            val nameOpt  = value(configDb.get(Entries.userName))
            val emailOpt = value(configDb.get(Entries.userEmail))
            val urlOpt   = value(configDb.get(Entries.userUrl))

            (nameOpt, emailOpt, urlOpt) match {
              case (Some(name), Some(email), Some(url)) =>
                logger.message(s"Using name $name, email $email, and URL $url from config")
                s"$name|$email|$url"
              case _ =>
                value {
                  Left {
                    new MissingPublishOptionError(
                      "developer",
                      "--developer",
                      "publish.developer",
                      configKeys = Seq(
                        Entries.userName.fullName,
                        Entries.userEmail.fullName,
                        Entries.userUrl.fullName
                      )
                    )
                  }
                }
            }
          case Some(value) =>
            value
        }

        OptionCheck.DefaultValue.simple(strValue, Nil, Nil)
      },
    OptionCheck.Kind.Extra
  )

  def checks(
    options: PublishSetupOptions,
    configDb: => ConfigDb,
    workspace: os.Path,
    coursierCache: Cache[Task],
    logger: Logger,
    backend: SttpBackend[Identity, Any]
  ) = Seq(
    orgCheck(options, workspace, logger),
    nameCheck(options, workspace, logger),
    computeVersionCheck(options, workspace, logger),
    repositoryCheck(options, logger),
    userCheck(options, configDb, logger),
    passwordCheck(options, configDb, logger),
    pgpSecretKeyCheck(options, coursierCache, configDb, logger, backend),
    licenseCheck(options, logger),
    urlCheck(options, workspace, logger),
    scmCheck(options, workspace, logger),
    developersCheck(options, configDb, logger)
  )

}
