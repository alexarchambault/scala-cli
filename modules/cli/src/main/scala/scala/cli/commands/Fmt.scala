package scala.cli.commands

import caseapp._
import dependency._

import scala.build.interactive.InteractiveFileOps
import scala.build.internal.{
  Constants,
  CustomCodeWrapper,
  ExternalBinaryParams,
  FetchExternalBinary,
  Runner
}
import scala.build.{CrossSources, Inputs, Logger, Sources}
import scala.cli.CurrentParams
import scala.cli.commands.util.FmtOptionsUtil._
import scala.cli.commands.util.SharedOptionsUtil._
import scala.cli.commands.util.VerbosityOptionsUtil._
import scala.util.control.NonFatal

object Fmt extends ScalaCommand[FmtOptions] {
  override def group: String                                             = "Main"
  override def sharedOptions(options: FmtOptions): Option[SharedOptions] = Some(options.shared)
  override def names: List[List[String]] = List(
    List("fmt"),
    List("format"),
    List("scalafmt")
  )

  private def getGitRoot(workspace: os.Path, logger: Logger): Option[String] =
    try {
      val result = os.proc("git", "rev-parse", "--show-toplevel").call(cwd = workspace).out.trim()
      Option(result)
    }
    catch {
      case NonFatal(e) =>
        logger.log(
          s"""Could not get root of the git repository.
             |Cause: $e""".stripMargin
        )
        None
    }

  // Based on scalafmt comment:
  // https://github.com/scalameta/scalafmt/blob/d0c11e98898334969f5f4dfc4bd511630cf00ab9/scalafmt-cli/src/main/scala/org/scalafmt/cli/CliArgParser.scala
  // First we look for the file in the cwd.
  // If not found or could not read, we go to the git root and look there.
  // If not found or could not read, we use the default version.
  def readVersionFromFile(workspace: os.Path, logger: Logger): (Option[String], Boolean) = {
    case class ScalafmtVersionConfig(version: String = "")
    object ScalafmtVersionConfig {
      lazy val default: ScalafmtVersionConfig = ScalafmtVersionConfig()
      implicit lazy val surface: metaconfig.generic.Surface[ScalafmtVersionConfig] =
        metaconfig.generic.deriveSurface[ScalafmtVersionConfig]
      implicit lazy val decoder: metaconfig.ConfDecoder[ScalafmtVersionConfig] =
        metaconfig.generic.deriveDecoder[ScalafmtVersionConfig](default)
    }

    val confName = ".scalafmt.conf"
    val pathMaybe = {
      logger.debug(s"Checking for $confName in cwd.")
      val confInCwd = workspace / confName
      if (os.exists(confInCwd)) Some(confInCwd)
      else {
        logger.debug(s"Checking for $confName in git root.")
        val gitRootMaybe       = getGitRoot(workspace, logger)
        val confInGitRootMaybe = gitRootMaybe.map(os.Path(_) / confName)
        confInGitRootMaybe.find(os.exists(_))
      }
    }

    val confContentMaybe = pathMaybe.flatMap { path =>
      val either = metaconfig.Hocon.parseInput[ScalafmtVersionConfig](
        metaconfig.Input.File(path.toNIO)
      ).toEither
      either.left.foreach(confErr => logger.log(confErr.toString()))
      either.toOption
    }

    val versionMaybe = confContentMaybe.collect {
      case conf if conf.version.trim.nonEmpty => conf.version
    }
    (versionMaybe, pathMaybe.isDefined)
  }

  def run(options: FmtOptions, args: RemainingArgs): Unit = {
    CurrentParams.verbosity = options.shared.logging.verbosity
    val interactive = options.shared.logging.verbosityOptions.interactiveInstance()
    val logger      = options.shared.logger

    // TODO If no input is given, just pass '.' to scalafmt?
    val (sourceFiles, workspace, inputsOpt) =
      if (args.all.isEmpty)
        (Seq(os.pwd), os.pwd, None)
      else {
        val i = options.shared.inputsOrExit(args)
        val s = i.sourceFiles().collect {
          case sc: Inputs.Script    => sc.path
          case sc: Inputs.ScalaFile => sc.path
        }
        (s, i.workspace, Some(i))
      }
    CurrentParams.workspaceOpt = Some(workspace)
    val (versionMaybe, confExists0) = readVersionFromFile(workspace, logger)
    val cache                       = options.shared.buildOptions().archiveCache
    val buildOptions                = options.buildOptions

    val (version, confExists) = versionMaybe.map((_, confExists0)).getOrElse {
      val errorMsg =
        s"""|Scalafmt requires explicitly specified version.
            |To configure the scalafmt version add the following line into .scalafmt.conf:
            |    version = ${Constants.defaultScalafmtVersion} """.stripMargin
      val msg = s"""|$errorMsg
                    |Do you want to add Scalafmt version to .scalafmt.conf file?""".stripMargin
      val scalafmtConfPath = workspace / ".scalafmt"
      val entry = s"version = ${Constants.defaultScalafmtVersion}${System.lineSeparator()}"
      InteractiveFileOps.appendToFile(interactive, msg, scalafmtConfPath, entry) { () =>
        System.err.println(errorMsg)
        sys.exit(1)
      }
      (Constants.defaultScalafmtVersion, true)
    }

    if (sourceFiles.isEmpty)
      logger.debug("No source files, not formatting anything")
    else {

      def scalaVerOpt = inputsOpt.flatMap { inputs =>
        val (crossSources, _) =
          CrossSources.forInputs(
            inputs,
            Sources.defaultPreprocessors(
              buildOptions.scriptOptions.codeWrapper.getOrElse(CustomCodeWrapper),
              buildOptions.archiveCache,
              buildOptions.internal.javaClassNameVersionOpt,
              () => buildOptions.javaHome().value.javaCommand
            ),
            logger
          ).orExit(logger)
        val sharedOptions = crossSources.sharedOptions(buildOptions)
        sharedOptions
          .scalaParams
          .orExit(logger)
          .map(_.scalaVersion)
      }

      def dialectOpt = options.dialect.map(_.trim).filter(_.nonEmpty).orElse {
        scalaVerOpt.flatMap {
          case v if v.startsWith("2.12.") => Some("Scala212")
          case v if v.startsWith("2.13.") => Some("Scala213")
          case v if v.startsWith("3.")    => Some("Scala3")
          case _                          => None
        }
      }

      val dialectArgs =
        if (options.scalafmtArg.isEmpty && !confExists)
          dialectOpt.toSeq.flatMap(dialect => Seq("--config-str", s"runner.dialect=$dialect"))
        else
          Nil

      val fmtCommand = options.scalafmtLauncher.filter(_.nonEmpty) match {
        case Some(launcher) =>
          Seq(launcher)
        case None =>
          val (url, changing) = options.binaryUrl(version)
          val params = ExternalBinaryParams(
            url,
            changing,
            "scalafmt",
            Seq(dep"${Constants.scalafmtOrganization}:${Constants.scalafmtName}:$version"),
            "org.scalafmt.cli.Cli"
          )
          FetchExternalBinary.fetch(
            params,
            cache,
            logger,
            () => buildOptions.javaHome().value.javaCommand
          )
            .orExit(logger)
            .command
      }

      logger.debug(s"Launching scalafmt with command $fmtCommand")

      val command = fmtCommand ++
        sourceFiles.map(_.toString) ++
        dialectArgs ++
        options.scalafmtCliOptions
      Runner.maybeExec(
        "scalafmt",
        command,
        logger,
        cwd = Some(workspace)
      ).waitFor()
    }
  }
}
