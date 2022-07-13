package scala.cli.commands

import caseapp._
import coursier.cache.FileCache
import dependency._

import scala.build.EitherCps.{either, value}
import scala.build._
import scala.build.errors.BuildException
import scala.build.internal.Runner
import scala.build.options.{BuildOptions, JavaOpt, MaybeScalaVersion, Scope}
import scala.cli.CurrentParams
import scala.cli.commands.run.RunMode
import scala.cli.commands.util.CommonOps._
import scala.cli.commands.util.SharedOptionsUtil._
import scala.util.Properties
import scala.cli.config.{ConfigDb, Keys}
import scala.cli.commands.packaging.Spark
import scala.cli.commands.util.CommonOps.SharedDirectoriesOptionsOps

object Repl extends ScalaCommand[ReplOptions] {
  override def group = "Main"
  override def names = List(
    List("console"),
    List("repl")
  )
  override def sharedOptions(options: ReplOptions) = Some(options.shared)

  def buildOptions(ops: ReplOptions): BuildOptions = {
    import ops._
    def ammoniteVersionOpt = ammoniteVersion.map(_.trim).filter(_.nonEmpty)

    val spark = runMode(ops) match {
      case _: RunMode.Spark => true
      case RunMode.Default  => false
    }

    val baseOptions = shared.buildOptions()
    baseOptions.copy(
      scalaOptions = baseOptions.scalaOptions.copy(
        scalaVersion = baseOptions.scalaOptions.scalaVersion
          .orElse(if (spark) Some(MaybeScalaVersion("2.12")) else None)
      ),
      javaOptions = baseOptions.javaOptions.copy(
        javaOpts =
          baseOptions.javaOptions.javaOpts ++
            sharedJava.allJavaOpts.map(JavaOpt(_)).map(Positioned.commandLine _) ++
            (if (spark) Seq(Positioned.none(JavaOpt("-Dscala.usejavacp=true"))) else Nil),
        jvmIdOpt = baseOptions.javaOptions.jvmIdOpt
          .orElse(if (spark) Some("8") else None)
      ),
      notForBloopOptions = baseOptions.notForBloopOptions.copy(
        replOptions = baseOptions.notForBloopOptions.replOptions.copy(
          useAmmoniteOpt = ammonite,
          ammoniteVersionOpt = ammoniteVersionOpt,
          ammoniteArgs = ammoniteArg
        )
      ),
      internalDependencies = baseOptions.internalDependencies.copy(
        addRunnerDependencyOpt = baseOptions.internalDependencies.addRunnerDependencyOpt
          .orElse(Some(false))
      ),
      internal = baseOptions.internal.copy(
        keepResolution = baseOptions.internal.keepResolution || spark
      )
    )
  }

  private def runMode(options: ReplOptions): RunMode.HasRepl = {
    def sparkReplOptions =
      options.predef.filter(_.trim.nonEmpty)
        .map(p => Seq("-I", p))
        .getOrElse(Nil)
    if (options.standaloneSpark) RunMode.StandaloneSparkSubmit(Nil, sparkReplOptions)
    else if (options.spark) RunMode.SparkSubmit(Nil, sparkReplOptions)
    else RunMode.Default
  }

  def run(options: ReplOptions, args: RemainingArgs): Unit = {
    CurrentParams.verbosity = options.shared.logging.verbosity
    def default = Inputs.default().getOrElse {
      Inputs.empty(Os.pwd)
    }
    val logger = options.shared.logger
    val inputs = options.shared.inputs(args.remaining, defaultInputs = () => Some(default)).orExit(logger)
    val programArgs = args.unparsed
    CurrentParams.workspaceOpt = Some(inputs.workspace)

    val initialBuildOptions = buildOptions(options)
    val threads             = BuildThreads.create()

    val compilerMaker = options.shared.compilerMaker(threads)

    val directories = options.shared.directories.directories

    def buildFailed(allowExit: Boolean): Unit = {
      System.err.println("Compilation failed")
      if (allowExit)
        sys.exit(1)
    }
    def buildCancelled(allowExit: Boolean): Unit = {
      System.err.println("Build cancelled")
      if (allowExit)
        sys.exit(1)
    }

    def doRunRepl(
      buildOptions: BuildOptions,
      artifacts: Artifacts,
      classDir: Option[os.Path],
      allowExit: Boolean,
      runMode: RunMode.HasRepl,
      buildOpt: Option[Build.Successful]
    ): Unit = {
      val res = runRepl(
        buildOptions,
        programArgs,
        artifacts,
        classDir,
        directories,
        logger,
        allowExit = allowExit,
        options.replDryRun,
        runMode,
        buildOpt
      )
      res match {
        case Left(ex) =>
          if (allowExit) logger.exit(ex)
          else logger.log(ex)
        case Right(()) =>
      }
    }

    val cross = options.compileCross.cross.getOrElse(false)
    val configDb = ConfigDb.open(options.shared.directories.directories)
      .orExit(logger)
    val actionableDiagnostics = configDb.get(Keys.actions).getOrElse(None)

    if (inputs.isEmpty) {
      val artifacts = initialBuildOptions.artifacts(logger, Scope.Main).orExit(logger)
      doRunRepl(
        initialBuildOptions,
        artifacts,
        None,
        allowExit = !options.watch.watchMode,
        runMode = runMode(options),
        buildOpt = None
      )
      if (options.watch.watchMode) {
        // nothing to watch, just wait for Ctrl+C
        WatchUtil.printWatchMessage()
        WatchUtil.waitForCtrlC()
      }
    }
    else if (options.watch.watchMode) {
      val watcher = Build.watch(
        inputs,
        initialBuildOptions,
        compilerMaker,
        None,
        logger,
        crossBuilds = cross,
        buildTests = false,
        partial = None,
        actionableDiagnostics = actionableDiagnostics,
        postAction = () => WatchUtil.printWatchMessage()
      ) { res =>
        for (builds <- res.orReport(logger))
          builds.main match {
            case s: Build.Successful =>
              doRunRepl(
                s.options,
                s.artifacts,
                s.outputOpt,
                allowExit = false,
                runMode = runMode(options),
                buildOpt = Some(s)
              )
            case _: Build.Failed    => buildFailed(allowExit = false)
            case _: Build.Cancelled => buildCancelled(allowExit = false)
          }
      }
      try WatchUtil.waitForCtrlC()
      finally watcher.dispose()
    }
    else {
      val builds =
        Build.build(
          inputs,
          initialBuildOptions,
          compilerMaker,
          None,
          logger,
          crossBuilds = cross,
          buildTests = false,
          partial = None,
          actionableDiagnostics = actionableDiagnostics
        )
          .orExit(logger)
      builds.main match {
        case s: Build.Successful =>
          doRunRepl(
            s.options,
            s.artifacts,
            s.outputOpt,
            allowExit = true,
            runMode = runMode(options),
            buildOpt = Some(s)
          )
        case _: Build.Failed    => buildFailed(allowExit = true)
        case _: Build.Cancelled => buildCancelled(allowExit = true)
      }
    }
  }

  private def runRepl(
    options: BuildOptions,
    programArgs: Seq[String],
    artifacts: Artifacts,
    classDir: Option[os.Path],
    directories: scala.build.Directories,
    logger: Logger,
    allowExit: Boolean,
    dryRun: Boolean,
    runMode: RunMode.HasRepl,
    buildOpt: Option[Build.Successful]
  ): Either[BuildException, Unit] = either {

    val scalaParams = artifacts.scalaOpt
      .getOrElse {
        sys.error("Expected Scala artifacts to be fetched")
      }
      .params

    runMode match {
      case RunMode.Default =>
        val cache = options.internal.cache.getOrElse(FileCache())
        val replArtifacts = value {
          if (options.notForBloopOptions.replOptions.useAmmonite)
            ReplArtifacts.ammonite(
              scalaParams,
              options.notForBloopOptions.replOptions.ammoniteVersion,
              artifacts.userDependencies,
              artifacts.extraClassPath,
              artifacts.extraSourceJars,
              logger,
              cache,
              directories
            )
          else
            ReplArtifacts.default(
              scalaParams,
              artifacts.userDependencies,
              artifacts.extraClassPath,
              logger,
              cache,
              options.finalRepositories
            )
        }

        // TODO Warn if some entries of artifacts.classPath were evicted in replArtifacts.replClassPath
        //      (should be artifacts whose version was bumped by Ammonite).

        // TODO Find the common namespace of all user classes, and import it all in the Ammonite session.

        // TODO Allow to disable printing the welcome banner and the "Loading..." message in Ammonite.

        val rootClasses = classDir
          .toSeq
          .flatMap(os.list(_))
          .filter(_.last.endsWith(".class"))
          .filter(os.isFile(_)) // just in case
          .map(_.last.stripSuffix(".class"))
          .sorted
        val warnRootClasses = rootClasses.nonEmpty &&
          options.notForBloopOptions.replOptions.useAmmonite
        if (warnRootClasses)
          logger.message(
            s"Warning: found classes defined in the root package (${rootClasses.mkString(", ")})." +
              " These will not be accessible from the REPL."
          )

        val additionalArgs =
          if (options.notForBloopOptions.replOptions.useAmmonite)
            options.notForBloopOptions.replOptions.ammoniteArgs
          else
            options.scalaOptions.scalacOptions.toSeq.map(_.value.value)

        val replArgs = additionalArgs ++ programArgs

        if (dryRun)
          logger.message("Dry run, not running REPL.")
        else {
          val retCode = Runner.runJvm(
            options.javaHome().value.javaCommand,
            replArtifacts.replJavaOpts ++ options.javaOptions.javaOpts.toSeq.map(_.value.value),
            classDir.toSeq ++ replArtifacts.replClassPath,
            replArtifacts.replMainClass,
            if (Properties.isWin)
              replArgs.map { a =>
                if (a.contains(" ")) "\"" + a.replace("\"", "\\\"") + "\""
                else a
              }
            else
              replArgs,
            logger,
            allowExecve = allowExit
          ).waitFor()
          if (retCode != 0)
            value(Left(new ReplError(retCode)))
        }

      case mode: RunMode.Spark =>
        val build = buildOpt.getOrElse {
          val ws      = os.temp.dir()
          val inputs  = Inputs.empty(ws)
          val sources = Sources(Nil, Nil, None, Nil, options)
          val scope   = Scope.Main
          Build.Successful(
            inputs = inputs,
            options = options,
            scalaParams = Some(scalaParams),
            scope = scope,
            sources = Sources(Nil, Nil, None, Nil, options),
            artifacts = artifacts,
            project = value(Build.buildProject(inputs, sources, Nil, options, None, scope, logger)),
            output = classDir.getOrElse(ws),
            diagnostics = None,
            generatedSources = Nil,
            isPartial = false
          )
        }
        // FIXME scalac options are ignored here
        val res = value {
          Run.runOnce(
            build = build,
            mainClass = "org.apache.spark.repl.Main",
            args = mode.replArgs,
            logger = logger,
            allowExecve = allowExit,
            runMode = mode.withSubmitArgs(programArgs),
            showCommand = dryRun,
            scratchDirOpt = None
          )
        }
        if (dryRun)
          logger.message("Dry run, not running REPL.")
        else {
          val (proc, hookOpt) = res match {
            case Left(_) =>
              // can only be left if showCommand == true, that is dryRun == true
              sys.error("Cannot happen")
            case Right(r) => r
          }
          val retCode =
            try proc.waitFor()
            finally hookOpt.foreach(_())
          if (retCode != 0)
            value(Left(new ReplError(retCode)))
        }
    }
  }

  final class ReplError(retCode: Int)
      extends BuildException(s"Failed to run REPL (exit code: $retCode)")
}
