package scala.cli.commands.publish

import org.eclipse.jgit.api.Git
import org.eclipse.jgit.storage.file.FileBasedConfig
import org.eclipse.jgit.util.FS

import scala.build.Logger
import scala.jdk.CollectionConverters._

object GitRepo {

  def userAndEmail(logger: Logger): (Option[String], Option[String]) = {
    val cfgFile = os.home / ".gitconfig"
    if (os.isFile(cfgFile)) {
      logger.debug(s"Found $cfgFile, trying to read user / email from it")
      val cfg     = new FileBasedConfig(cfgFile.toIO, FS.DETECTED)
      val nameOpt = Option(cfg.getString("user", null, "name"))
      logger.debug(s"got name $nameOpt")
      val mailOpt = Option(cfg.getString("user", null, "email"))
      logger.debug(s"got email $mailOpt")
      (nameOpt, mailOpt)
    }
    else {
      logger.debug(s"$cfgFile not found, not reading user / email from it")
      (None, None)
    }
  }

  def isGitRepo(workspace: os.Path): Boolean =
    os.isDir(workspace / ".git")

  def ghRepoOrgName(
    workspace: os.Path,
    logger: Logger
  ): Either[GitRepoError, (String, String)] = {

    val gitHubRemotes =
      if (isGitRepo(workspace)) {

        val remoteList = Git.open(workspace.toIO).remoteList().call().asScala
        logger.debug(s"Found ${remoteList.length} remotes in Git repo $workspace")

        remoteList
          .iterator
          .flatMap { remote =>
            val name = remote.getName
            remote
              .getURIs
              .asScala
              .iterator
              .map(_.toASCIIString)
              .flatMap(maybeGhOrgName)
              .map((name, _))
          }
          .toVector
      }
      else
        Vector.empty

    gitHubRemotes match {
      case Seq() =>
        Left(new GitRepoError(s"Cannot determine GitHub organization and name for $workspace"))
      case Seq((_, orgName)) =>
        Right(orgName)
      case more =>
        val map = more.toMap
        map.get("upstream").orElse(map.get("origin")).toRight {
          new GitRepoError(s"Cannot determine default GitHub organization and name for $workspace")
        }
    }
  }

  private def maybeGhOrgName(uri: String): Option[(String, String)] =
    if (uri.startsWith("https://github.com/")) {
      val pathPart = uri.stripPrefix("https://github.com/").stripSuffix(".git")
      pathPart.split("/") match {
        case Array(org, name) => Some((org, name))
        case _                => None
      }
    }
    else
      None
}
