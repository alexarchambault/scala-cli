package scala.cli.commands.publish

import coursier.cache.Cache
import coursier.util.Task

import scala.build.Logger
import scala.build.errors.BuildException
import scala.cli.commands.pgp.{PgpCreateExternal, PgpKeyIdExternal}

class ThrowawayPgpSecretCreate {
  def call(
    pubKey: String,
    secKey: String,
    mail: String,
    quiet: Boolean,
    password: String,
    cache: Cache[Task],
    logger: Logger
  ): Either[BuildException, Int] = {
    val quietOptions = Nil
    (new PgpCreateExternal).tryRun(
      cache,
      None,
      Seq(
        "pgp",
        "create",
        "--pub-dest",
        pubKey.toString,
        "--secret-dest",
        secKey.toString,
        "--email",
        mail,
        "--password",
        s"env:SCALA_CLI_RANDOM_KEY_PASSWORD"
      ) ++ quietOptions,
      Map("SCALA_CLI_RANDOM_KEY_PASSWORD" -> password),
      logger,
      allowExecve = false
    )
  }

  def keyId(
    key: String,
    cache: Cache[Task],
    logger: Logger
  ): Either[BuildException, String] = {
    val keyPath = os.temp(key, prefix = "key", suffix = ".pub", perms = "rwx------")
    try {
      (new PgpKeyIdExternal).output(
        cache,
        None,
        Seq("pgp", "key-id", keyPath.toString),
        Map(),
        logger
      )
    }
    finally os.remove(keyPath)
  }

}
