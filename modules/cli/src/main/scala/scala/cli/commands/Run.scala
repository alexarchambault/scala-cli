package scala.cli.commands

import caseapp._

import java.util.concurrent.CompletableFuture

import scala.build.EitherCps.{either, value}
import scala.build.errors.BuildException
import scala.build.internal.{Constants, Runner, ScalaJsLinkerConfig}
import scala.build.options.{BuildOptions, JavaOpt, Platform}
import scala.build.{Build, BuildThreads, Inputs, Logger, Positioned}
import scala.cli.CurrentParams
import scala.cli.commands.packaging.Spark
import scala.cli.commands.run.RunMode
import scala.cli.commands.util.MainClassOptionsUtil._
import scala.cli.commands.util.SharedOptionsUtil._
import scala.cli.commands.{Package => PackageCmd}
import scala.cli.internal.ProcUtil
import scala.cli.packaging.Library
import scala.util.Properties

object Run extends ScalaCommand[RunOptions] {
  override def group = "Main"

  override def sharedOptions(options: RunOptions): Option[SharedOptions] = Some(options.shared)

  private def runMode(options: RunOptions): RunMode =
    if (options.standaloneSpark.getOrElse(false) && !options.sparkSubmit.contains(false))
      RunMode.StandaloneSparkSubmit
    else if (options.sparkSubmit.getOrElse(false))
      RunMode.SparkSubmit
    else if (options.hadoopJar)
      RunMode.HadoopJar
    else
      RunMode.Default

  private def scratchDirOpt(options: RunOptions): Option[os.Path] =
    options.scratchDir
      .filter(_.trim.nonEmpty)
      .map(os.Path(_, os.pwd))

  def run(options: RunOptions, args: RemainingArgs): Unit = {
    maybePrintGroupHelp(options)
    run(
      options,
      args.remaining,
      args.unparsed,
      () => Inputs.default()
    )
  }

  def buildOptions(options: RunOptions): BuildOptions = {
    import options._
    val baseOptions = shared.buildOptions(
      enableJmh = benchmarking.jmh.contains(true),
      jmhVersion = benchmarking.jmhVersion
    )
    baseOptions.copy(
      mainClass = mainClass.mainClass,
      javaOptions = baseOptions.javaOptions.copy(
        javaOpts =
          baseOptions.javaOptions.javaOpts ++
            sharedJava.allJavaOpts.map(JavaOpt(_)).map(Positioned.commandLine),
        jvmIdOpt = baseOptions.javaOptions.jvmIdOpt.orElse {
          runMode(options) match {
            case RunMode.StandaloneSparkSubmit | RunMode.SparkSubmit | RunMode.HadoopJar =>
              Some("8")
            case RunMode.Default => None
          }
        }
      ),
      internalDependencies = baseOptions.internalDependencies.copy(
        addRunnerDependencyOpt = baseOptions.internalDependencies.addRunnerDependencyOpt.orElse {
          runMode(options) match {
            case RunMode.StandaloneSparkSubmit | RunMode.SparkSubmit | RunMode.HadoopJar =>
              Some(false)
            case RunMode.Default => None
          }
        }
      ),
      internal = baseOptions.internal.copy(
        keepResolution = baseOptions.internal.keepResolution || {
          runMode(options) match {
            case RunMode.StandaloneSparkSubmit | RunMode.SparkSubmit | RunMode.HadoopJar => true
            case RunMode.Default                                                         => false
          }
        }
      ),
      notForBloopOptions = baseOptions.notForBloopOptions.copy(
        runWithManifest = options.useManifest
      )
    )
  }

  def run(
    options: RunOptions,
    inputArgs: Seq[String],
    programArgs: Seq[String],
    defaultInputs: () => Option[Inputs]
  ): Unit = {
    CurrentParams.verbosity = options.shared.logging.verbosity
    val initialBuildOptions = buildOptions(options)
    maybePrintSimpleScalacOutput(options, initialBuildOptions)

    val inputs = options.shared.inputsOrExit(inputArgs, defaultInputs = defaultInputs)
    CurrentParams.workspaceOpt = Some(inputs.workspace)
    val logger  = options.shared.logger
    val threads = BuildThreads.create()

    val compilerMaker = options.shared.compilerMaker(threads)

    def maybeRun(
      build: Build.Successful,
      allowTerminate: Boolean,
      runMode: RunMode,
      showCommand: Boolean,
      scratchDirOpt: Option[os.Path]
    ): Either[BuildException, Option[(Process, CompletableFuture[_])]] = either {
      val potentialMainClasses = build.foundMainClasses()
      if (options.mainClass.mainClassLs.contains(true))
        value {
          options.mainClass
            .maybePrintMainClasses(potentialMainClasses, shouldExit = allowTerminate)
            .map(_ => None)
        }
      else {
        val processOrCommand = value {
          maybeRunOnce(
            build,
            programArgs,
            logger,
            allowExecve = allowTerminate,
            jvmRunner = build.artifacts.hasJvmRunner,
            potentialMainClasses,
            runMode,
            showCommand,
            scratchDirOpt
          )
        }

        processOrCommand match {
          case Right((process, onExitOpt)) =>
            val onExitProcess = process.onExit().thenApply { p1 =>
              val retCode = p1.exitValue()
              onExitOpt.foreach(_())
              if (retCode != 0)
                if (allowTerminate)
                  sys.exit(retCode)
                else {
                  val red      = Console.RED
                  val lightRed = "\u001b[91m"
                  val reset    = Console.RESET
                  System.err.println(
                    s"${red}Program exited with return code $lightRed$retCode$red.$reset"
                  )
                }
            }

            Some((process, onExitProcess))

          case Left(command) =>
            for (arg <- command)
              println(arg)
            None
        }
      }
    }

    val cross = options.compileCross.cross.getOrElse(false)
    SetupIde.runSafe(
      options.shared,
      inputs,
      logger,
      Some(name),
      inputArgs
    )
    if (CommandUtils.shouldCheckUpdate)
      Update.checkUpdateSafe(logger)

    if (options.watch.watchMode) {
      var processOpt = Option.empty[(Process, CompletableFuture[_])]
      val watcher = Build.watch(
        inputs,
        initialBuildOptions,
        compilerMaker,
        None,
        logger,
        crossBuilds = cross,
        buildTests = false,
        partial = None,
        postAction = () => WatchUtil.printWatchMessage()
      ) { res =>
        for ((process, onExitProcess) <- processOpt) {
          onExitProcess.cancel(true)
          ProcUtil.interruptProcess(process, logger)
        }
        res.orReport(logger).map(_.main).foreach {
          case s: Build.Successful =>
            for ((proc, _) <- processOpt) // If the process doesn't exit, send SIGKILL
              if (proc.isAlive) ProcUtil.forceKillProcess(proc, logger)
            val maybeProcess = maybeRun(
              s,
              allowTerminate = false,
              runMode = runMode(options),
              showCommand = options.command,
              scratchDirOpt = scratchDirOpt(options)
            )
              .orReport(logger)
              .flatten
            if (options.watch.restart)
              processOpt = maybeProcess
            else
              for ((proc, onExit) <- maybeProcess)
                ProcUtil.waitForProcess(proc, onExit)
          case _: Build.Failed =>
            System.err.println("Compilation failed")
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
          partial = None
        )
          .orExit(logger)
      builds.main match {
        case s: Build.Successful =>
          val res = maybeRun(
            s,
            allowTerminate = true,
            runMode = runMode(options),
            showCommand = options.command,
            scratchDirOpt = scratchDirOpt(options)
          )
            .orExit(logger)
          for ((process, onExit) <- res)
            ProcUtil.waitForProcess(process, onExit)
        case _: Build.Failed =>
          System.err.println("Compilation failed")
          sys.exit(1)
      }
    }
  }

  private def maybeRunOnce(
    build: Build.Successful,
    args: Seq[String],
    logger: Logger,
    allowExecve: Boolean,
    jvmRunner: Boolean,
    potentialMainClasses: Seq[String],
    runMode: RunMode,
    showCommand: Boolean,
    scratchDirOpt: Option[os.Path]
  ): Either[BuildException, Either[Seq[String], (Process, Option[() => Unit])]] = either {

    val mainClassOpt = build.options.mainClass.filter(_.nonEmpty) // trim it too?
      .orElse {
        if (build.options.jmhOptions.runJmh.contains(false)) Some("org.openjdk.jmh.Main")
        else None
      }
    val mainClass = mainClassOpt match {
      case Some(cls) => cls
      case None      => value(build.retainedMainClass(potentialMainClasses, logger))
    }
    val verbosity = build.options.internal.verbosity.getOrElse(0).toString

    val (finalMainClass, finalArgs) =
      if (jvmRunner) (Constants.runnerMainClass, mainClass +: verbosity +: args)
      else (mainClass, args)
    val res = runOnce(
      build,
      finalMainClass,
      finalArgs,
      logger,
      allowExecve,
      runMode,
      showCommand,
      scratchDirOpt
    )
    value(res)
  }

  private def runOnce(
    build: Build.Successful,
    mainClass: String,
    args: Seq[String],
    logger: Logger,
    allowExecve: Boolean,
    runMode: RunMode,
    showCommand: Boolean,
    scratchDirOpt: Option[os.Path]
  ): Either[BuildException, Either[Seq[String], (Process, Option[() => Unit])]] = either {

    build.options.platform.value match {
      case Platform.JS =>
        val esModule =
          build.options.scalaJsOptions.moduleKindStr.exists(m => m == "es" || m == "esmodule")

        val linkerConfig = build.options.scalaJsOptions.linkerConfig(logger)
        val jsDest = {
          val delete = scratchDirOpt.isEmpty
          scratchDirOpt.foreach(os.makeDir.all(_))
          os.temp(
            dir = scratchDirOpt.orNull,
            prefix = "main",
            suffix = if (esModule) ".mjs" else ".js",
            deleteOnExit = delete
          )
        }
        val res =
          Package.linkJs(
            build,
            jsDest,
            Some(mainClass),
            addTestInitializer = false,
            linkerConfig,
            build.options.scalaJsOptions.fullOpt,
            build.options.scalaJsOptions.noOpt.getOrElse(false),
            logger,
            scratchDirOpt
          ).map { outputPath =>
            val jsDom = build.options.scalaJsOptions.dom.getOrElse(false)
            if (showCommand)
              Left(Runner.jsCommand(outputPath.toIO, args, jsDom = jsDom))
            else {
              val process = Runner.runJs(
                outputPath.toIO,
                args,
                logger,
                allowExecve = allowExecve,
                jsDom = jsDom,
                sourceMap = build.options.scalaJsOptions.emitSourceMaps,
                esModule = esModule
              )
              process.onExit().thenApply(_ => if (os.exists(jsDest)) os.remove(jsDest))
              Right((process, None))
            }
          }
        value(res)
      case Platform.Native =>
        withNativeLauncher(
          build,
          mainClass,
          logger
        ) { launcher =>
          if (showCommand)
            Left(launcher.toString +: args)
          else {
            val proc = Runner.runNative(
              launcher.toIO,
              args,
              logger,
              allowExecve = allowExecve
            )
            Right((proc, None))
          }
        }
      case Platform.JVM =>
        runMode match {
          case RunMode.Default =>
            if (showCommand) {
              val command = Runner.jvmCommand(
                build.options.javaHome().value.javaCommand,
                build.options.javaOptions.javaOpts.toSeq.map(_.value.value),
                build.fullClassPath,
                mainClass,
                args,
                useManifest = build.options.notForBloopOptions.runWithManifest,
                scratchDirOpt = scratchDirOpt
              )
              Left(command)
            }
            else {
              val proc = Runner.runJvm(
                build.options.javaHome().value.javaCommand,
                build.options.javaOptions.javaOpts.toSeq.map(_.value.value),
                build.fullClassPath,
                mainClass,
                args,
                logger,
                allowExecve = allowExecve,
                useManifest = build.options.notForBloopOptions.runWithManifest,
                scratchDirOpt = scratchDirOpt
              )
              Right((proc, None))
            }
          case RunMode.SparkSubmit =>
            // FIXME Get Spark.sparkModules via provided settings?
            val providedModules = Spark.sparkModules
            val providedFiles =
              value(PackageCmd.providedFiles(build, providedModules, logger)).toSet
            val customSubmitOptions: Seq[String] = Nil // ???
            val depCp        = build.dependencyClassPath.filterNot(providedFiles)
            val javaHomeInfo = build.options.javaHome().value
            val javaOpts     = build.options.javaOptions.javaOpts.toSeq.map(_.value.value)
            val ext = if (Properties.isWin) ".cmd" else ""
            val submitCommand: String =
              Option(System.getenv("SPARK_HOME"))
                .map(os.Path(_, os.pwd))
                .map(_ / "bin" / s"spark-submit$ext")
                .filter(os.exists(_))
                .map(_.toString)
                .getOrElse("spark-submit")
            val jarsArgs =
              if (depCp.isEmpty) Nil
              else Seq("--jars", depCp.mkString(","))

            scratchDirOpt.foreach(os.makeDir.all(_))
            val library = os.temp(
              Library.libraryJar(build),
              dir = scratchDirOpt.orNull,
              deleteOnExit = scratchDirOpt.isEmpty,
              prefix = "spark-job",
              suffix = ".jar"
            )

            val finalCommand =
              Seq(submitCommand, "--class", mainClass) ++
                jarsArgs ++
                javaOpts.flatMap(opt => Seq("--driver-java-options", opt)) ++
                customSubmitOptions ++
                Seq(library.toString) ++
                args
            val envUpdates = javaHomeInfo.envUpdates(sys.env)
            if (showCommand)
              Left(Runner.envCommand(envUpdates) ++ finalCommand)
            else {
              val proc =
                if (allowExecve)
                  Runner.maybeExec("spark-submit", finalCommand, logger, extraEnv = envUpdates)
                else
                  Runner.run(finalCommand, logger, extraEnv = envUpdates)
              Right((
                proc,
                if (scratchDirOpt.isEmpty) Some(() => os.remove(library, checkExists = true))
                else None
              ))
            }
          case RunMode.StandaloneSparkSubmit =>
            // FIXME Get Spark.sparkModules via provided settings?
            val providedModules = Spark.sparkModules
            val sparkClassPath  = value(PackageCmd.providedFiles(build, providedModules, logger))

            scratchDirOpt.foreach(os.makeDir.all(_))
            val library = os.temp(
              Library.libraryJar(build),
              dir = scratchDirOpt.orNull,
              deleteOnExit = scratchDirOpt.isEmpty,
              prefix = "spark-job",
              suffix = ".jar"
            )

            val customSubmitOptions: Seq[String] = Nil // ???
            val finalMainClass                   = "org.apache.spark.deploy.SparkSubmit"
            val depCp        = build.dependencyClassPath.filterNot(sparkClassPath.toSet)
            val javaHomeInfo = build.options.javaHome().value
            val javaOpts     = build.options.javaOptions.javaOpts.toSeq.map(_.value.value)
            val jarsArgs =
              if (depCp.isEmpty) Nil
              else Seq("--jars", depCp.mkString(","))
            val finalArgs =
              Seq("--class", mainClass) ++
                jarsArgs ++
                javaOpts.flatMap(opt => Seq("--driver-java-options", opt)) ++
                customSubmitOptions ++
                Seq(library.toString) ++
                args
            val envUpdates = javaHomeInfo.envUpdates(sys.env)
            if (showCommand) {
              val command = Runner.jvmCommand(
                javaHomeInfo.javaCommand,
                javaOpts,
                sparkClassPath,
                finalMainClass,
                finalArgs,
                extraEnv = envUpdates,
                useManifest = build.options.notForBloopOptions.runWithManifest,
                scratchDirOpt = scratchDirOpt
              )
              Left(command)
            }
            else {
              val proc = Runner.runJvm(
                javaHomeInfo.javaCommand,
                javaOpts,
                sparkClassPath,
                finalMainClass,
                finalArgs,
                logger,
                allowExecve = allowExecve,
                extraEnv = envUpdates,
                useManifest = build.options.notForBloopOptions.runWithManifest,
                scratchDirOpt = scratchDirOpt
              )
              Right((
                proc,
                if (scratchDirOpt.isEmpty) Some(() => os.remove(library, checkExists = true))
                else None
              ))
            }
          case RunMode.HadoopJar =>
            // FIXME Get Spark.hadoopModules via provided settings?
            val providedModules = Spark.hadoopModules
            scratchDirOpt.foreach(os.makeDir.all(_))
            val assembly = os.temp(
              dir = scratchDirOpt.orNull,
              prefix = "hadoop-job",
              suffix = ".jar",
              deleteOnExit = scratchDirOpt.isEmpty
            )
            value {
              PackageCmd.assembly(
                build,
                assembly,
                mainClass,
                providedModules,
                withPreamble = false,
                () => (),
                logger
              )
            }

            val javaOpts = build.options.javaOptions.javaOpts.toSeq.map(_.value.value)
            val extraEnv =
              if (javaOpts.isEmpty) Map[String, String]()
              else
                Map(
                  "HADOOP_CLIENT_OPTS" -> javaOpts.mkString(" ") // no escaping…
                )
            val hadoopJarCommand = Seq("hadoop", "jar")
            val finalCommand =
              hadoopJarCommand ++ Seq(assembly.toString, mainClass) ++ args
            if (showCommand)
              Left(Runner.envCommand(extraEnv) ++ finalCommand)
            else {
              val proc =
                if (allowExecve)
                  Runner.maybeExec("hadoop", finalCommand, logger, extraEnv = extraEnv)
                else
                  Runner.run(finalCommand, logger, extraEnv = extraEnv)
              Right((
                proc,
                if (scratchDirOpt.isEmpty) Some(() => os.remove(assembly, checkExists = true))
                else None
              ))
            }
        }
    }
  }

  def withLinkedJs[T](
    build: Build.Successful,
    mainClassOpt: Option[String],
    addTestInitializer: Boolean,
    config: ScalaJsLinkerConfig,
    fullOpt: Boolean,
    noOpt: Boolean,
    logger: Logger,
    esModule: Boolean
  )(f: os.Path => T): Either[BuildException, T] = {
    val dest = os.temp(prefix = "main", suffix = if (esModule) ".mjs" else ".js")
    try Package.linkJs(
        build,
        dest,
        mainClassOpt,
        addTestInitializer,
        config,
        fullOpt,
        noOpt,
        logger
      ).map { outputPath =>
        f(outputPath)
      }
    finally if (os.exists(dest)) os.remove(dest)
  }

  def withNativeLauncher[T](
    build: Build.Successful,
    mainClass: String,
    logger: Logger
  )(f: os.Path => T): T = {
    val dest = build.inputs.nativeWorkDir / s"main${if (Properties.isWin) ".exe" else ""}"
    Package.buildNative(build, mainClass, dest, logger)
    f(dest)
  }
}
