package scala.cli.commands.pgp

import caseapp.core.RemainingArgs
import sttp.client3._
import sttp.model.Uri

import scala.cli.commands.ScalaCommand
import scala.cli.commands.publish.KeyServer
import scala.cli.commands.util.CommonOps._
import scala.cli.commands.util.ScalaCliSttpBackend

object PgpPush extends ScalaCommand[PgpPushOptions] {

  override def hidden = true
  override def inSipScala = false
  override def names = List(
    List("pgp", "push")
  )

  def defaultServer =
    // wouldn't https://keyserver.ubuntu.com work as well (https > http)
    uri"http://keyserver.ubuntu.com:11371"

  def run(options: PgpPushOptions, args: RemainingArgs): Unit = {

    val logger = options.logging.logger
    val backend = ScalaCliSttpBackend.httpURLConnection(logger)

    val keyServerUri = options.shared.serverUriOptOrExit(logger).getOrElse {
      defaultServer
    }

    val all = args.all

    if (!options.allowEmpty && all.isEmpty) {
      System.err.println("No key passed as argument.")
      sys.exit(1)
    }

    val addEndpoint = keyServerUri.addPath("pks", "add")

    for (key <- all) {
      val path = os.Path(key, os.pwd)
      if (!os.exists(path)) {
        System.err.println(s"Error: $key not found")
        sys.exit(1)
      }
      val keyContent = os.read(path)

      if (!keyContent.contains("-----BEGIN PGP PUBLIC KEY BLOCK-----"))
        if (options.force) {
          if (logger.verbosity >= 0)
            System.err.println(
              s"Warning: $key doesn't look like a PGP public key, proceeding anyway."
            )
        } else {
          System.err.println(
            s"Error: $key doesn't look like a PGP public key. " +
            "Use --force to force uploading it anyway."
          )
          sys.exit(1)
        }

      val res = KeyServer.add(
        keyContent,
        addEndpoint,
        backend
      )

      res match {
        case Left(error) =>
          System.err.println(s"Error uploading key to $keyServerUri.")
          if (logger.verbosity >= 0)
            System.err.println(s"Server response: $error")
          sys.exit(1)
        case Right(_) =>
          logger.message(s"Key $key uploaded")
      }
    }
  }
}
