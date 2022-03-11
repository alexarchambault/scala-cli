package scala.cli.commands

import caseapp._
import coursier.launcher._
import org.scalajs.linker.interface.StandardConfig
import packager.config._
import packager.deb.DebianPackage
import packager.docker.DockerPackage
import packager.mac.dmg.DmgPackage
import packager.mac.pkg.PkgPackage
import packager.rpm.RedHatPackage
import packager.windows.WindowsPackage

import java.io.{ByteArrayOutputStream, File, OutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path}
import java.util.jar.{Attributes => JarAttributes, JarOutputStream}
import java.util.zip.{ZipEntry, ZipOutputStream}

import scala.annotation.tailrec
import scala.build.EitherCps.{either, value}
import scala.build._
import scala.build.errors.{BuildException, ScalaNativeBuildError}
import scala.build.internal.{NativeBuilderHelper, Runner, ScalaJsConfig}
import scala.build.options.{PackageType, Platform}
import scala.cli.CurrentParams
import scala.cli.commands.OptionsHelper._
import scala.cli.errors.{GraalVMNativeImageError, ScalaJsLinkingError}
import scala.cli.internal.{ProcUtil, ScalaJsLinker}
import scala.util.Properties

object Package extends ScalaCommand[PackageOptions] {
  override def group                                  = "Main"
  override def sharedOptions(options: PackageOptions) = Some(options.shared)
  def run(options: PackageOptions, args: RemainingArgs): Unit = {
    maybePrintGroupHelp(options)
    CurrentParams.verbosity = options.shared.logging.verbosity
    val inputs = options.shared.inputsOrExit(args)
    CurrentParams.workspaceOpt = Some(inputs.workspace)

    // FIXME mainClass encoding has issues with special chars, such as '-'

    val initialBuildOptions = options.buildOptions
    val logger              = options.shared.logger
    val threads             = BuildThreads.create()

    val compilerMaker = options.shared.compilerMaker(threads)

    val cross = options.compileCross.cross.getOrElse(false)

    if (options.watch.watch) {
      var expectedModifyEpochSecondOpt = Option.empty[Long]
      val watcher = Build.watch(
        inputs,
        initialBuildOptions,
        compilerMaker,
        logger,
        crossBuilds = cross,
        buildTests = false,
        partial = None,
        postAction = () => WatchUtil.printWatchMessage()
      ) { res =>
        res.orReport(logger).map(_.main).foreach {
          case s: Build.Successful =>
            val mtimeDestPath = doPackage(
              logger,
              options.output.filter(_.nonEmpty),
              options.force,
              s,
              expectedModifyEpochSecondOpt,
              args.unparsed
            )
              .orReport(logger)
            for (valueOpt <- mtimeDestPath)
              expectedModifyEpochSecondOpt = valueOpt
          case _: Build.Failed =>
            System.err.println("Compilation failed")
          case _: Build.Cancelled =>
            System.err.println("Build cancelled")
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
          logger,
          crossBuilds = cross,
          buildTests = false,
          partial = None
        )
          .orExit(logger)
      builds.main match {
        case s: Build.Successful =>
          doPackage(
            logger,
            options.output.filter(_.nonEmpty),
            options.force,
            s,
            None,
            args.unparsed
          )
            .orExit(logger)
        case _: Build.Failed =>
          System.err.println("Compilation failed")
          sys.exit(1)
        case _: Build.Cancelled =>
          System.err.println("Build cancelled")
          sys.exit(1)
      }
    }
  }

  private def doPackage(
    logger: Logger,
    outputOpt: Option[String],
    force: Boolean,
    build: Build.Successful,
    expectedModifyEpochSecondOpt: Option[Long],
    extraArgs: Seq[String]
  ): Either[BuildException, Option[Long]] = either {

    // FIXME We'll probably need more refined rules if we start to support extra Scala.JS or Scala Native specific types
    val packageType =
      if (build.options.notForBloopOptions.packageOptions.isDockerEnabled)
        PackageType.Docker
      else if (build.options.platform.value == Platform.JS)
        PackageType.Js
      else if (build.options.platform.value == Platform.Native)
        PackageType.Native
      else
        build.options.notForBloopOptions.packageOptions.packageTypeOpt
          .getOrElse(PackageType.Bootstrap)

    // TODO When possible, call alreadyExistsCheck() before compiling stuff

    def extension = packageType match {
      case PackageType.LibraryJar                             => ".jar"
      case PackageType.SourceJar                              => ".jar"
      case PackageType.Assembly                               => ".jar"
      case PackageType.Js                                     => ".js"
      case PackageType.Debian                                 => ".deb"
      case PackageType.Dmg                                    => ".dmg"
      case PackageType.Pkg                                    => ".pkg"
      case PackageType.Rpm                                    => ".rpm"
      case PackageType.Msi                                    => ".msi"
      case PackageType.Native if Properties.isWin             => ".exe"
      case PackageType.GraalVMNativeImage if Properties.isWin => ".exe"
      case _ if Properties.isWin                              => ".bat"
      case _                                                  => ""
    }
    def defaultName = packageType match {
      case PackageType.LibraryJar                             => "library.jar"
      case PackageType.SourceJar                              => "source.jar"
      case PackageType.Assembly                               => "app.jar"
      case PackageType.Js                                     => "app.js"
      case PackageType.Debian                                 => "app.deb"
      case PackageType.Dmg                                    => "app.dmg"
      case PackageType.Pkg                                    => "app.pkg"
      case PackageType.Rpm                                    => "app.rpm"
      case PackageType.Msi                                    => "app.msi"
      case PackageType.Native if Properties.isWin             => "app.exe"
      case PackageType.GraalVMNativeImage if Properties.isWin => "app.exe"
      case _ if Properties.isWin                              => "app.bat"
      case _                                                  => "app"
    }

    val dest = outputOpt
      .orElse {
        build.sources.mainClass
          .map(n => n.drop(n.lastIndexOf('.') + 1))
          .map(_.stripSuffix("_sc"))
          .map(_ + extension)
      }
      .getOrElse(defaultName)
    val destPath = os.Path(dest, Os.pwd)
    val printableDest =
      if (destPath.startsWith(Os.pwd)) "." + File.separator + destPath.relativeTo(Os.pwd).toString
      else destPath.toString

    def alreadyExistsCheck(): Unit = {
      val alreadyExists = !force &&
        os.exists(destPath) &&
        expectedModifyEpochSecondOpt.forall(exp => os.mtime(destPath) != exp)
      if (alreadyExists) {
        val msg =
          if (expectedModifyEpochSecondOpt.isEmpty) s"$printableDest already exists"
          else s"$printableDest was overwritten by another process"
        System.err.println(s"Error: $msg. Pass -f or --force to force erasing it.")
        sys.exit(1)
      }
    }

    alreadyExistsCheck()

    def mainClass: Either[BuildException, String] =
      build.options.mainClass match {
        case Some(cls) => Right(cls)
        case None      => build.retainedMainClass
      }

    val packageOptions = build.options.notForBloopOptions.packageOptions

    packageType match {
      case PackageType.Bootstrap =>
        bootstrap(build, destPath, value(mainClass), () => alreadyExistsCheck())
      case PackageType.LibraryJar =>
        val content = libraryJar(build)
        alreadyExistsCheck()
        if (force) os.write.over(destPath, content)
        else os.write(destPath, content)
      case PackageType.SourceJar =>
        val now     = System.currentTimeMillis()
        val content = sourceJar(build, now)
        alreadyExistsCheck()
        if (force) os.write.over(destPath, content)
        else os.write(destPath, content)
      case PackageType.Assembly =>
        assembly(build, destPath, value(mainClass), () => alreadyExistsCheck())

      case PackageType.Js =>
        value(buildJs(build, destPath, value(mainClass), logger))

      case PackageType.Native =>
        buildNative(build, destPath, value(mainClass), logger)

      case PackageType.GraalVMNativeImage =>
        buildGraalVMNativeImage(build, destPath, value(mainClass), extraArgs, logger)

      case nativePackagerType: PackageType.NativePackagerType =>
        val bootstrapPath = os.temp.dir(prefix = "scala-packager") / "app"
        bootstrap(build, bootstrapPath, value(mainClass), () => alreadyExistsCheck())
        val sharedSettings = SharedSettings(
          sourceAppPath = bootstrapPath,
          version = packageOptions.packageVersion,
          force = force,
          outputPath = destPath,
          logoPath = packageOptions.logoPath,
          launcherApp = packageOptions.launcherApp
        )

        lazy val debianSettings = DebianSettings(
          shared = sharedSettings,
          maintainer = packageOptions.maintainer.mandatory("--maintainer", "debian"),
          description = packageOptions.description.mandatory("--description", "debian"),
          debianConflicts = packageOptions.debianOptions.conflicts,
          debianDependencies = packageOptions.debianOptions.dependencies,
          architecture = packageOptions.debianOptions.architecture.mandatory(
            "--deb-architecture",
            "debian"
          )
        )

        lazy val macOSSettings = MacOSSettings(
          shared = sharedSettings,
          identifier =
            packageOptions.macOSidentifier.mandatory("--identifier-parameter", "macOs")
        )

        lazy val redHatSettings = RedHatSettings(
          shared = sharedSettings,
          description = packageOptions.description.mandatory("--description", "redHat"),
          license =
            packageOptions.redHatOptions.license.mandatory("--license", "redHat"),
          release =
            packageOptions.redHatOptions.release.mandatory("--release", "redHat"),
          rpmArchitecture = packageOptions.redHatOptions.architecture.mandatory(
            "--rpm-architecture",
            "redHat"
          )
        )

        lazy val windowsSettings = WindowsSettings(
          shared = sharedSettings,
          maintainer = packageOptions.maintainer.mandatory("--maintainer", "windows"),
          licencePath = packageOptions.windowsOptions.licensePath.mandatory(
            "--licence-path",
            "windows"
          ),
          productName = packageOptions.windowsOptions.productName.mandatory(
            "--product-name",
            "windows"
          ),
          exitDialog = packageOptions.windowsOptions.exitDialog,
          suppressValidation =
            packageOptions.windowsOptions.suppressValidation.getOrElse(false),
          extraConfigs = packageOptions.windowsOptions.extraConfig,
          is64Bits = packageOptions.windowsOptions.is64Bits.getOrElse(true),
          installerVersion = packageOptions.windowsOptions.installerVersion
        )

        nativePackagerType match {
          case PackageType.Debian =>
            DebianPackage(debianSettings).build()
          case PackageType.Dmg =>
            DmgPackage(macOSSettings).build()
          case PackageType.Pkg =>
            PkgPackage(macOSSettings).build()
          case PackageType.Rpm =>
            RedHatPackage(redHatSettings).build()
          case PackageType.Msi =>
            WindowsPackage(windowsSettings).build()
        }
      case PackageType.Docker =>
        docker(build, value(mainClass), logger)
    }

    if (packageType.runnable.nonEmpty)
      logger.message {
        if (packageType.runnable.contains(true))
          s"Wrote $dest, run it with" + System.lineSeparator() +
            "  " + printableDest
        else if (packageType == PackageType.Js)
          s"Wrote $dest, run it with" + System.lineSeparator() +
            "  node " + printableDest
        else
          s"Wrote $dest"
      }

    val mTimeDestPathOpt = if (packageType.runnable.isEmpty) None else Some(os.mtime(destPath))
    mTimeDestPathOpt
  }

  def libraryJar(
    build: Build.Successful,
    mainClassOpt: Option[String] = None
  ): Array[Byte] = {

    val baos = new ByteArrayOutputStream

    val manifest = new java.util.jar.Manifest
    manifest.getMainAttributes.put(JarAttributes.Name.MANIFEST_VERSION, "1.0")
    for (mainClass <- mainClassOpt.orElse(build.sources.mainClass) if mainClass.nonEmpty)
      manifest.getMainAttributes.put(JarAttributes.Name.MAIN_CLASS, mainClass)

    var zos: ZipOutputStream = null

    try {
      zos = new JarOutputStream(baos, manifest)
      for (path <- os.walk(build.output) if os.isFile(path)) {
        val name         = path.relativeTo(build.output).toString
        val lastModified = os.mtime(path)
        val ent          = new ZipEntry(name)
        ent.setLastModifiedTime(FileTime.fromMillis(lastModified))

        val content = os.read.bytes(path)
        ent.setSize(content.length)

        zos.putNextEntry(ent)
        zos.write(content)
        zos.closeEntry()
      }
    }
    finally if (zos != null) zos.close()

    baos.toByteArray
  }

  private val generatedSourcesPrefix = os.rel / "META-INF" / "generated"
  def sourceJar(build: Build.Successful, defaultLastModified: Long): Array[Byte] = {

    val baos                 = new ByteArrayOutputStream
    var zos: ZipOutputStream = null

    def fromSimpleSources = build.sources.paths.iterator.map {
      case (path, relPath) =>
        val lastModified = os.mtime(path)
        val content      = os.read.bytes(path)
        (relPath, content, lastModified)
    }

    def fromGeneratedSources = build.sources.inMemory.iterator.flatMap { inMemSource =>
      val lastModified = inMemSource.originalPath match {
        case Right((_, origPath)) => os.mtime(origPath)
        case Left(_)              => defaultLastModified
      }
      val originalOpt = inMemSource.originalPath.toOption.collect {
        case (subPath, origPath) if subPath != inMemSource.generatedRelPath =>
          val origContent = os.read.bytes(origPath)
          (subPath, origContent, lastModified)
      }
      val prefix = if (originalOpt.isEmpty) os.rel else generatedSourcesPrefix
      val generated = (
        prefix / inMemSource.generatedRelPath,
        inMemSource.generatedContent.getBytes(StandardCharsets.UTF_8),
        lastModified
      )
      Iterator(generated) ++ originalOpt.iterator
    }

    def paths = fromSimpleSources ++ fromGeneratedSources

    try {
      zos = new ZipOutputStream(baos)
      for ((relPath, content, lastModified) <- paths) {
        val name = relPath.toString
        val ent  = new ZipEntry(name)
        ent.setLastModifiedTime(FileTime.fromMillis(lastModified))
        ent.setSize(content.length)

        zos.putNextEntry(ent)
        zos.write(content)
        zos.closeEntry()
      }
    }
    finally if (zos != null) zos.close()

    baos.toByteArray
  }

  private def docker(
    build: Build.Successful,
    mainClass: String,
    logger: Logger
  ): Unit = {
    val packageOptions = build.options.notForBloopOptions.packageOptions

    if (build.options.platform.value == Platform.Native && (Properties.isMac || Properties.isWin)) {
      System.err.println(
        "Package scala native application to docker image is not supported on MacOs and Windows"
      )
      sys.exit(1)
    }

    val exec = build.options.platform.value match {
      case Platform.JVM    => Some("sh")
      case Platform.JS     => Some("node")
      case Platform.Native => None
    }
    val from = packageOptions.dockerOptions.from.getOrElse {
      build.options.platform.value match {
        case Platform.JVM    => "openjdk:17-slim"
        case Platform.JS     => "node"
        case Platform.Native => "debian:stable-slim"
      }
    }
    val repository = packageOptions.dockerOptions.imageRepository.mandatory(
      "--docker-image-repository",
      "docker"
    )
    val tag = packageOptions.dockerOptions.imageTag.getOrElse("latest")

    val dockerSettings = DockerSettings(
      from = from,
      registry = packageOptions.dockerOptions.imageRegistry,
      repository = repository,
      tag = Some(tag),
      exec = exec
    )

    val appPath = os.temp.dir(prefix = "scala-cli-docker") / "app"
    build.options.platform.value match {
      case Platform.JVM    => bootstrap(build, appPath, mainClass, () => ())
      case Platform.JS     => buildJs(build, appPath, mainClass, logger)
      case Platform.Native => buildNative(build, appPath, mainClass, logger)
    }

    logger.message(
      "Started building docker image with your application, it might take some time"
    )

    DockerPackage(appPath, dockerSettings).build()

    logger.message(
      "Built docker image, run it with" + System.lineSeparator() +
        s"  docker run $repository:$tag"
    )
  }

  private def buildJs(
    build: Build.Successful,
    destPath: os.Path,
    mainClass: String,
    logger: Logger
  ): Either[BuildException, Unit] = {
    val linkerConfig = build.options.scalaJsOptions.linkerConfig(logger)
    linkJs(build, destPath, Some(mainClass), addTestInitializer = false, linkerConfig, logger)
  }

  private def buildNative(
    build: Build.Successful,
    destPath: os.Path,
    mainClass: String,
    logger: Logger
  ): Unit = {
    val workDir =
      build.options.scalaNativeOptions.nativeWorkDir(
        build.inputs.workspace,
        build.inputs.projectName
      )

    buildNative(build, mainClass, destPath, workDir, logger)
  }

  private def buildGraalVMNativeImage(
    build: Build.Successful,
    destPath: os.Path,
    mainClass: String,
    extraArgs: Seq[String],
    logger: Logger
  ): Unit = {
    val workDir =
      build.options.nativeImageWorkDir(build.inputs.workspace, build.inputs.projectName)

    buildNativeImage(build, mainClass, destPath, workDir, extraArgs, logger)
  }

  private def bootstrap(
    build: Build.Successful,
    destPath: os.Path,
    mainClass: String,
    alreadyExistsCheck: () => Unit
  ): Unit = {
    val byteCodeZipEntries = os.walk(build.output)
      .filter(os.isFile(_))
      .map { path =>
        val name         = path.relativeTo(build.output).toString
        val content      = os.read.bytes(path)
        val lastModified = os.mtime(path)
        val ent          = new ZipEntry(name)
        ent.setLastModifiedTime(FileTime.fromMillis(lastModified))
        ent.setSize(content.length)
        (ent, content)
      }

    // TODO Generate that in memory
    val tmpJar = Files.createTempFile(destPath.last.stripSuffix(".jar"), ".jar")
    val tmpJarParams = Parameters.Assembly()
      .withExtraZipEntries(byteCodeZipEntries)
      .withMainClass(mainClass)
    AssemblyGenerator.generate(tmpJarParams, tmpJar)
    val tmpJarContent = os.read.bytes(os.Path(tmpJar))
    Files.deleteIfExists(tmpJar)

    def dependencyEntries =
      build.artifacts.artifacts.map {
        case (url, path) =>
          if (build.options.notForBloopOptions.packageOptions.isStandalone)
            ClassPathEntry.Resource(path.last, os.mtime(path), os.read.bytes(path))
          else
            ClassPathEntry.Url(url)
      }
    val byteCodeEntry = ClassPathEntry.Resource(s"${destPath.last}-content.jar", 0L, tmpJarContent)

    val allEntries    = Seq(byteCodeEntry) ++ dependencyEntries
    val loaderContent = coursier.launcher.ClassLoaderContent(allEntries)
    val preamble = Preamble()
      .withOsKind(Properties.isWin)
      .callsItself(Properties.isWin)
    val params = Parameters.Bootstrap(Seq(loaderContent), mainClass)
      .withDeterministic(true)
      .withPreamble(preamble)

    alreadyExistsCheck()
    BootstrapGenerator.generate(params, destPath.toNIO)
    ProcUtil.maybeUpdatePreamble(destPath)
  }

  private def assembly(
    build: Build.Successful,
    destPath: os.Path,
    mainClass: String,
    alreadyExistsCheck: () => Unit
  ): Unit = {
    val byteCodeZipEntries = os.walk(build.output)
      .filter(os.isFile(_))
      .map { path =>
        val name         = path.relativeTo(build.output).toString
        val content      = os.read.bytes(path)
        val lastModified = os.mtime(path)
        val ent          = new ZipEntry(name)
        ent.setLastModifiedTime(FileTime.fromMillis(lastModified))
        ent.setSize(content.length)
        (ent, content)
      }

    val preamble = Preamble()
      .withOsKind(Properties.isWin)
      .callsItself(Properties.isWin)
    val params = Parameters.Assembly()
      .withExtraZipEntries(byteCodeZipEntries)
      .withFiles(build.artifacts.artifacts.map(_._2.toIO))
      .withMainClass(mainClass)
      .withPreamble(preamble)
    alreadyExistsCheck()
    AssemblyGenerator.generate(params, destPath.toNIO)
    ProcUtil.maybeUpdatePreamble(destPath)
  }

  def withLibraryJar[T](build: Build.Successful, fileName: String = "library")(f: Path => T): T = {
    val mainJarContent = libraryJar(build)
    val mainJar        = Files.createTempFile(fileName.stripSuffix(".jar"), ".jar")
    try {
      Files.write(mainJar, mainJarContent)
      f(mainJar)
    }
    finally Files.deleteIfExists(mainJar)
  }

  def withSourceJar[T](
    build: Build.Successful,
    defaultLastModified: Long,
    fileName: String = "library"
  )(f: Path => T): T = {
    val jarContent = sourceJar(build, defaultLastModified)
    val jar        = Files.createTempFile(fileName.stripSuffix(".jar"), "-sources.jar")
    try {
      Files.write(jar, jarContent)
      f(jar)
    }
    finally Files.deleteIfExists(jar)
  }

  def linkJs(
    build: Build.Successful,
    dest: os.Path,
    mainClassOpt: Option[String],
    addTestInitializer: Boolean,
    config: StandardConfig,
    logger: Logger
  ): Either[BuildException, Unit] =
    withLibraryJar(build, dest.last.toString.stripSuffix(".jar")) { mainJar =>
      val classPath  = mainJar +: build.artifacts.classPath.map(_.toNIO)
      val linkingDir = os.temp.dir(prefix = "scala-cli-js-linking")
      (new ScalaJsLinker).link(
        classPath.toArray,
        mainClassOpt.orNull,
        addTestInitializer,
        new ScalaJsConfig(config),
        linkingDir.toNIO,
        logger.scalaJsLogger
      )
      val relMainJs = os.rel / "main.js"
      val mainJs    = linkingDir / relMainJs
      if (os.exists(mainJs)) {
        os.copy(mainJs, dest, replaceExisting = true)
        os.remove.all(linkingDir)
        Right(())
      }
      else {
        val found = os.walk(linkingDir).map(_.relativeTo(linkingDir))
        Left(new ScalaJsLinkingError(relMainJs, found))
      }
    }

  def buildNative(
    build: Build.Successful,
    mainClass: String,
    dest: os.Path,
    nativeWorkDir: os.Path,
    logger: Logger
  ): Unit = {

    val cliOptions = build.options.scalaNativeOptions.configCliOptions()

    os.makeDir.all(nativeWorkDir)

    val cacheData =
      NativeBuilderHelper.getCacheData(
        build,
        cliOptions,
        dest,
        nativeWorkDir
      )

    if (cacheData.changed)
      withLibraryJar(build, dest.last.stripSuffix(".jar")) { mainJar =>

        val classpath = build.fullClassPath.map(_.toString) :+ mainJar.toString()
        val args =
          cliOptions ++
            logger.scalaNativeCliInternalLoggerOptions ++
            List[String](
              "--outpath",
              dest.toString(),
              "--workdir",
              nativeWorkDir.toString(),
              "--main",
              mainClass
            ) ++ classpath

        val exitCode =
          Runner.runJvm(
            build.options.javaHome().value.javaCommand,
            build.options.javaOptions.javaOpts.toSeq.map(_.value.value),
            build.artifacts.scalaNativeCli.map(_.toIO),
            "scala.scalanative.cli.ScalaNativeLd",
            args,
            logger
          )
        if (exitCode == 0)
          NativeBuilderHelper.updateProjectAndOutputSha(dest, nativeWorkDir, cacheData.projectSha)
        else
          throw new ScalaNativeBuildError
      }
  }

  private def ensureHasNativeImageCommand(
    graalVMHome: os.Path,
    logger: Logger
  ): os.Path = {

    val ext         = if (Properties.isWin) ".cmd" else ""
    val nativeImage = graalVMHome / "bin" / s"native-image$ext"

    if (os.exists(nativeImage))
      logger.debug(s"$nativeImage found")
    else {
      val proc = os.proc(graalVMHome / "bin" / s"gu$ext", "install", "native-image")
      logger.debug(s"$nativeImage not found, running ${proc.command.flatMap(_.value)}")
      proc.call(stdin = os.Inherit, stdout = os.Inherit)
      if (!os.exists(nativeImage))
        logger.message(
          s"Seems gu install command didn't install $nativeImage, trying to run it anyway"
        )
    }

    nativeImage
  }

  private def maybeWithManifestClassPath[T](
    createManifest: Boolean,
    classPath: Seq[os.Path]
  )(
    f: Seq[os.Path] => T
  ): T = {
    var toDeleteOpt = Option.empty[os.Path]

    try {
      val finalCp =
        if (createManifest) {
          import java.util.jar._
          val manifest = new Manifest
          val attributes = manifest.getMainAttributes
          attributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")
          attributes.put(Attributes.Name.CLASS_PATH, classPath.map(_.toString).mkString(" "))
          val jarFile = os.temp(prefix = "classpathJar", suffix = ".jar")
          toDeleteOpt = Some(jarFile)
          var os0: OutputStream = null
          var jos: JarOutputStream = null
          try {
            os0 = os.write.outputStream(jarFile)
            jos = new JarOutputStream(os0, manifest)
          }
          finally {
            if (jos != null)
              jos.close()
            if (os0 != null)
              os0.close()
          }
          Seq(jarFile)
        } else
          classPath

      f(finalCp)
    }
    finally {
      for (toDelete <- toDeleteOpt)
        os.remove(toDelete)
    }
  }

  private def vcVersions = Seq("2022", "2019", "2017")
  private def vcEditions = Seq("Enterprise", "Community", "BuildTools")
  lazy val vcvarsCandidates = Option(System.getenv("VCVARSALL")) ++ {
    for {
      isX86 <- Seq(false, true)
      version <- vcVersions
      edition <- vcEditions
    } yield {
      val programFiles = if (isX86) "Program Files (x86)" else "Program Files"
      """C:\""" + programFiles + """\Microsoft Visual Studio\""" + version + "\\" + edition + """\VC\Auxiliary\Build\vcvars64.bat"""
    }
  }

  private def vcvarsOpt: Option[os.Path] =
    vcvarsCandidates
      .iterator
      .map(os.Path(_, os.pwd))
      .filter(os.exists(_))
      .take(1)
      .toList
      .headOption

  private def runFromVcvarsBat(
    command: Seq[String],
    vcvars: os.Path,
    workingDir: os.Path
  ): Int = {
    pprint.stderr.log(vcvars)
    val escapedCommand = command.map {
      case s if s.contains(" ") => "\"" + s + "\""
      case s => s
    }
    // chcp 437 sometimes needed, see https://github.com/oracle/graal/issues/2522
    val script =
     s"""chcp 437
        |@call "$vcvars"
        |if %errorlevel% neq 0 exit /b %errorlevel%
        |@call ${escapedCommand.mkString(" ")}
        |""".stripMargin
    pprint.stderr.log(script)
    pprint.stderr.log(sys.env.toVector.filter(_._1.toLowerCase(java.util.Locale.ROOT).contains("path")))
    val scriptPath = workingDir / "run-native-image.bat"
    os.write.over(scriptPath, script.getBytes, createFolders = true)

    val finalCommand = Seq("cmd", "/c", scriptPath.toString)
    val res = os.proc(finalCommand).call(
      cwd = workingDir,
      check = false,
      stdin = os.Inherit,
      stdout = os.Inherit
    )

    res.exitCode
  }

  private def availableDriveLetter(): Char = {

    @tailrec
    def helper(from: Char): Char =
      if (from > 'Z') sys.error("Cannot find free drive letter")
      else {
        val p = os.Path(s"$from:" + "\\")
        if (os.exists(p)) helper((from + 1).toChar)
        else from
      }

    helper('D')
  }

  private def maybeWithShorterGraalvmHome[T](
    currentHome: os.Path
  )(
    f: os.Path => T
  ): T = {
    pprint.stderr.log(currentHome)
    if (Properties.isWin && currentHome.toString.length >= 180) {
      val driveLetter = availableDriveLetter()
      pprint.stderr.log(driveLetter)
      val setupCommand = s"""subst $driveLetter: "$currentHome""""
      val disableScript = s"""subst $driveLetter: /d"""

      os.proc("cmd", "/c", setupCommand).call(stdin = os.Inherit, stdout = os.Inherit)
      try {
        f(os.Path(s"$driveLetter:" + "\\"))
      }
      finally {
        os.proc("cmd", "/c", disableScript).call(stdin = os.Inherit, stdout = os.Inherit)
      }
    }
    else
      f(currentHome)
  }

  def buildNativeImage(
    build: Build.Successful,
    mainClass: String,
    dest: os.Path,
    nativeImageWorkDir: os.Path,
    extraOptions: Seq[String],
    logger: Logger
  ): Unit = {

    os.makeDir.all(nativeImageWorkDir)

    val jvmId = build.options.notForBloopOptions.packageOptions.nativeImageOptions.jvmId
    val options = build.options.copy(
      javaOptions = build.options.javaOptions.copy(
        jvmIdOpt = Some(jvmId)
      )
    )

    val javaHome = options.javaHome().value

    val cacheData = NativeBuilderHelper.getCacheData(
      build,
      s"--java-home=${javaHome.javaHome.toString}" :: "--" :: extraOptions.toList,
      dest,
      nativeImageWorkDir
    )

    if (cacheData.changed)
      withLibraryJar(build, dest.last.stripSuffix(".jar")) { mainJar =>

        val originalClasspath = build.fullClassPath :+ mainJar
        maybeWithManifestClassPath(
          createManifest = Properties.isWin,
          classPath = originalClasspath.map(os.Path(_, os.pwd))
        ) { classPath =>
          val args = extraOptions ++ Seq(
            s"-H:Path=${dest / os.up}",
            s"-H:Name=${dest.last}",
            "-cp",
            classPath.map(_.toString).mkString(File.pathSeparator),
            mainClass
          )

          maybeWithShorterGraalvmHome(javaHome.javaHome) { graalVMHome =>

            pprint.stderr.log(graalVMHome)

            val nativeImageCommand = ensureHasNativeImageCommand(graalVMHome, logger)
            val command = nativeImageCommand.toString +: args

            val exitCode =
              if (Properties.isWin)
                vcvarsOpt match {
                  case Some(vcvars) =>
                    runFromVcvarsBat(command, vcvars, nativeImageWorkDir)
                  case None =>
                    Runner.run("unused", command, logger, cwd = Some(nativeImageWorkDir))
                }
              else
                Runner.run("unused", command, logger, cwd = Some(nativeImageWorkDir))
            if (exitCode == 0)
              NativeBuilderHelper.updateProjectAndOutputSha(
                dest,
                nativeImageWorkDir,
                cacheData.projectSha
              )
            else
              throw new GraalVMNativeImageError
          }
        }
      }
    else
      logger.message("Found cached native image binary.")
  }
}
