package scala.build.options

import coursier.cache.{ArchiveCache, FileCache}
import coursier.core.Version
import coursier.jvm.{JavaHome, JvmCache, JvmIndex}
import coursier.util.{Artifact, Task}
import dependency._

import java.math.BigInteger
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.security.MessageDigest

import scala.build.EitherCps.{either, value}
import scala.build.blooprifle.VersionUtil.parseJavaVersion
import scala.build.errors.{
  BuildException,
  Diagnostic,
  InvalidBinaryScalaVersionError,
  NoValidScalaVersionFoundError,
  UnsupportedScalaVersionError
}
import scala.build.internal.Constants._
import scala.build.internal.CsLoggerUtil._
import scala.build.internal.{OsLibc, StableScalaVersion, Util}
import scala.build.options.validation.BuildOptionsRule
import scala.build.{Artifacts, Logger, Os, Position, Positioned}
import scala.util.Properties
import scala.util.control.NonFatal

final case class BuildOptions(
  scalaOptions: ScalaOptions = ScalaOptions(),
  scalaJsOptions: ScalaJsOptions = ScalaJsOptions(),
  scalaNativeOptions: ScalaNativeOptions = ScalaNativeOptions(),
  internalDependencies: InternalDependenciesOptions = InternalDependenciesOptions(),
  javaOptions: JavaOptions = JavaOptions(),
  jmhOptions: JmhOptions = JmhOptions(),
  classPathOptions: ClassPathOptions = ClassPathOptions(),
  scriptOptions: ScriptOptions = ScriptOptions(),
  internal: InternalOptions = InternalOptions(),
  mainClass: Option[String] = None,
  testOptions: TestOptions = TestOptions(),
  notForBloopOptions: PostBuildOptions = PostBuildOptions()
) {

  lazy val platform: Positioned[Platform] =
    scalaOptions.platform.getOrElse(Positioned(List(Position.Custom("DEFAULT")), Platform.JVM))

  lazy val projectParams: Either[BuildException, Seq[String]] = either {
    val platform0 = platform.value match {
      case Platform.JVM    => "JVM"
      case Platform.JS     => "Scala.JS"
      case Platform.Native => "Scala Native"
    }
    Seq(s"Scala ${value(scalaParams).scalaVersion}", platform0)
  }

  def addRunnerDependency: Option[Boolean] =
    internalDependencies.addRunnerDependencyOpt
      .orElse(if (platform.value == Platform.JVM) None else Some(false))

  private def scalaLibraryDependencies: Either[BuildException, Seq[AnyDependency]] = either {
    if (platform.value != Platform.Native && scalaOptions.addScalaLibrary.getOrElse(true)) {
      val scalaParams0 = value(scalaParams)
      val lib =
        if (scalaParams0.scalaVersion.startsWith("3."))
          dep"org.scala-lang::scala3-library::${scalaParams0.scalaVersion}"
        else
          dep"org.scala-lang:scala-library:${scalaParams0.scalaVersion}"
      Seq(lib)
    }
    else Nil
  }

  private def maybeJsDependencies: Either[BuildException, Seq[AnyDependency]] = either {
    if (platform.value == Platform.JS)
      scalaJsOptions.jsDependencies(value(scalaParams).scalaVersion)
    else Nil
  }
  private def maybeNativeDependencies: Either[BuildException, Seq[AnyDependency]] = either {
    if (platform.value == Platform.Native)
      scalaNativeOptions.nativeDependencies(value(scalaParams).scalaVersion)
    else Nil
  }
  private def dependencies: Either[BuildException, Seq[Positioned[AnyDependency]]] = either {
    value(maybeJsDependencies).map(Positioned.none(_)) ++
      value(maybeNativeDependencies).map(Positioned.none(_)) ++
      value(scalaLibraryDependencies).map(Positioned.none(_)) ++
      classPathOptions.extraDependencies.toSeq
  }

  private def semanticDbPlugins: Either[BuildException, Seq[AnyDependency]] = either {
    val generateSemDbs = scalaOptions.generateSemanticDbs.getOrElse(false) &&
      value(scalaParams).scalaVersion.startsWith("2.")
    if (generateSemDbs)
      Seq(
        dep"$semanticDbPluginOrganization:::$semanticDbPluginModuleName:$semanticDbPluginVersion"
      )
    else
      Nil
  }

  private def maybeJsCompilerPlugins: Either[BuildException, Seq[AnyDependency]] = either {
    if (platform.value == Platform.JS)
      scalaJsOptions.compilerPlugins(value(scalaParams).scalaVersion)
    else Nil
  }
  private def maybeNativeCompilerPlugins: Seq[AnyDependency] =
    if (platform.value == Platform.Native) scalaNativeOptions.compilerPlugins
    else Nil
  def compilerPlugins: Either[BuildException, Seq[Positioned[AnyDependency]]] = either {
    value(maybeJsCompilerPlugins).map(Positioned.none(_)) ++
      maybeNativeCompilerPlugins.map(Positioned.none(_)) ++
      value(semanticDbPlugins).map(Positioned.none(_)) ++
      scalaOptions.compilerPlugins
  }

  private def semanticDbJavacPlugins: Either[BuildException, Seq[AnyDependency]] = either {
    val generateSemDbs = scalaOptions.generateSemanticDbs.getOrElse(false)
    if (generateSemDbs)
      Seq(
        dep"$semanticDbJavacPluginOrganization:$semanticDbJavacPluginModuleName:$semanticDbJavacPluginVersion"
      )
    else
      Nil
  }

  def javacPluginDependencies: Either[BuildException, Seq[Positioned[AnyDependency]]] = either {
    value(semanticDbJavacPlugins).map(Positioned.none(_)) ++
      javaOptions.javacPluginDependencies
  }

  def allExtraJars: Seq[Path] =
    classPathOptions.extraClassPath.map(_.toNIO)
  def allExtraCompileOnlyJars: Seq[Path] =
    classPathOptions.extraCompileOnlyJars.map(_.toNIO)
  def allExtraSourceJars: Seq[Path] =
    classPathOptions.extraSourceJars.map(_.toNIO)

  private def addJvmTestRunner: Boolean =
    platform.value == Platform.JVM &&
    internalDependencies.addTestRunnerDependency
  private def addJsTestBridge: Option[String] =
    if (platform.value == Platform.JS && internalDependencies.addTestRunnerDependency)
      Some(scalaJsOptions.finalVersion)
    else None
  private def addNativeTestInterface: Option[String] = {
    val doAdd =
      platform.value == Platform.Native &&
      internalDependencies.addTestRunnerDependency &&
      Version("0.4.3").compareTo(Version(scalaNativeOptions.finalVersion)) <= 0
    if (doAdd) Some(scalaNativeOptions.finalVersion)
    else None
  }

  lazy val finalCache = internal.cache.getOrElse(FileCache())
  // This might download a JVM if --jvm … is passed or no system JVM is installed

  case class JavaHomeInfo(javaCommand: String, version: Int)

  private lazy val javaCommand0: Positioned[JavaHomeInfo] = {
    val javaHome = javaHomeLocation()
    val ext      = if (Properties.isWin) ".exe" else ""
    val javaCmd  = (javaHome.value / "bin" / s"java$ext").toString

    val javaVersionOutput = os.proc(javaCmd, "-version").call(
      cwd = os.pwd,
      stdout = os.Pipe,
      stderr = os.Pipe,
      mergeErrIntoOut = true
    ).out.text().trim()
    val javaVersion = parseJavaVersion(javaVersionOutput).getOrElse {
      throw new Exception(s"Could not parse java version from output: $javaVersionOutput")
    }

    Positioned(javaHome.positions, JavaHomeInfo(javaCmd, javaVersion))
  }

  private def jvmIndexOs = javaOptions.jvmIndexOs.getOrElse(OsLibc.jvmIndexOs)

  def javaHomeLocationOpt(): Option[Positioned[os.Path]] =
    javaOptions.javaHomeOpt
      .orElse {
        if (javaOptions.jvmIdOpt.isEmpty)
          sys.props.get("java.home").map(p =>
            Positioned(Position.Custom("java.home prop"), os.Path(p, Os.pwd))
          )
        else None
      }
      .orElse {
        javaOptions.jvmIdOpt.map { jvmId =>
          implicit val ec = finalCache.ec
          finalCache.logger.use {
            val enforceLiberica =
              jvmIndexOs == "linux-musl" && jvmId.forall(c => c.isDigit || c == '.' || c == '-')
            val jvmId0 =
              if (enforceLiberica)
                s"liberica:$jvmId" // FIXME Workaround, until this is automatically handled by coursier-jvm
              else
                jvmId
            val javaHomeManager0 = javaHomeManager
              .withMessage(s"Downloading JVM $jvmId0")
            val path =
              try javaHomeManager0.get(jvmId0).unsafeRun()
              catch {
                case NonFatal(e) => throw new Exception(e)
              }
            Positioned(Position.CommandLine("--jvm"), os.Path(path))
          }
        }
      }

  def javaHomeLocation(): Positioned[os.Path] =
    javaHomeLocationOpt().getOrElse {
      val jvmId = OsLibc.defaultJvm(jvmIndexOs)
      val javaHomeManager0 = javaHomeManager
        .withMessage(s"Downloading JVM $jvmId")
      implicit val ec = finalCache.ec
      finalCache.logger.use {
        val path =
          try javaHomeManager0.get(jvmId).unsafeRun()
          catch {
            case NonFatal(e) => throw new Exception(e)
          }
        Positioned(Position.Custom("OsLibc.defaultJvm"), os.Path(path))
      }
    }

  // used when downloading fails
  private def defaultStableScalaVersions =
    Seq(defaultScala212Version, defaultScala213Version, defaultScalaVersion)

  private def latestSupportedScalaVersion(): Seq[Version] = {

    val cache = finalCache.withMessage("Getting list of Scala CLI-supported Scala versions")
    val supportedScalaVersionsUrl = scalaOptions.scalaVersionsUrl

    val task = {
      val art = Artifact(supportedScalaVersionsUrl).withChanging(true)
      cache.file(art).run.flatMap {
        case Left(e) => Task.fail(e)
        case Right(f) =>
          Task.delay {
            val content = os.read(os.Path(f, Os.pwd))
            upickle.default.read[Seq[StableScalaVersion]](content)
          }
      }
    }

    val scalaCliVersion = version
    val launchersTask   = cache.logger.using(task)

    //  If an error occurred while downloading stable versions,
    //  it uses stable scala versions from Deps.sc
    val supportedScalaVersions =
      launchersTask.attempt.unsafeRun()(cache.ec) match {
        case Left(_) =>
          // FIXME Log the exception
          defaultStableScalaVersions
        case Right(versions) =>
          versions
            .find(_.scalaCliVersion == scalaCliVersion)
            .map(_.supportedScalaVersions)
            .getOrElse {
              // FIXME Log that: logger.debug(s"Couldn't find Scala CLI version $scalaCliVersion in $versions")
              defaultStableScalaVersions
            }
      }

    supportedScalaVersions
      .map(Version(_))
      .sorted
      .reverse
  }

  def javaHome(): Positioned[JavaHomeInfo] = javaCommand0

  lazy val javaHomeManager = {
    val indexUrl = javaOptions.jvmIndexOpt.getOrElse(JvmIndex.coursierIndexUrl)
    val indexTask = {
      val cache = finalCache.withMessage("Downloading JVM index")
      cache.logger.using {
        JvmIndex.load(cache, indexUrl)
      }
    }
    val jvmCache = JvmCache()
      .withIndex(indexTask)
      .withArchiveCache(
        ArchiveCache().withCache(
          finalCache.withMessage("Downloading JVM")
        )
      )
      .withOs(jvmIndexOs)
      .withArchitecture(javaOptions.jvmIndexArch.getOrElse(JvmIndex.defaultArchitecture()))
    JavaHome().withCache(jvmCache)
  }

  def finalRepositories: Seq[String] =
    classPathOptions.extraRepositories ++ internal.localRepository.toSeq

  private def computeScalaVersions(
    scalaVersion: Option[String],
    scalaBinaryVersion: Option[String]
  ): Either[BuildException, (String, String)] = either {
    lazy val allVersions = {
      import coursier._
      import scala.concurrent.ExecutionContext.{global => ec}
      val modules = {
        def scala2 = mod"org.scala-lang:scala-library"
        // No unstable, that *ought* not to be a problem down-the-line…?
        def scala3 = mod"org.scala-lang:scala3-library_3"
        if (scalaVersion.contains("2") || scalaVersion.exists(_.startsWith("2."))) Seq(scala2)
        else if (scalaVersion.contains("3") || scalaVersion.exists(_.startsWith("3."))) Seq(scala3)
        else Seq(scala2, scala3)
      }
      def isStable(v: String): Boolean =
        !v.endsWith("-NIGHTLY") && !v.contains("-RC")
      def moduleVersions(mod: Module): Seq[String] = {
        val cache = finalCache.withMessage("Getting Scala version list")
        val res = cache.logger.use {
          try Versions(cache)
            .withModule(mod)
            .result()
            .unsafeRun()(ec)
          catch {
            case NonFatal(e) => throw new Exception(e)
          }
        }
        res.versions.available.filter(isStable)
      }
      modules.flatMap(moduleVersions).distinct
    }
    def matchNewestScalaVersion(sv: Option[String]) = {
      lazy val maxSupportedScalaVersions = latestSupportedScalaVersion()

      sv match {
        case Some(sv0) =>
          val prefix           = if (sv0.endsWith(".")) sv0 else sv0 + "."
          val matchingVersions = allVersions.filter(_.startsWith(prefix)).map(Version(_))
          if (matchingVersions.isEmpty)
            Left(new InvalidBinaryScalaVersionError(sv0))
          else {
            val validMaxVersions = maxSupportedScalaVersions
              .filter(_.repr.startsWith(prefix))
            val validMatchingVersions = {
              val filtered = matchingVersions.filter(v => validMaxVersions.exists(v <= _))
              if (filtered.isEmpty) matchingVersions
              else filtered
            }
            if (validMatchingVersions.isEmpty)
              Left(new UnsupportedScalaVersionError(sv0))
            else
              Right(validMatchingVersions.max.repr)
          }
        case None =>
          val validVersions = allVersions
            .map(Version(_))
            .filter(v => maxSupportedScalaVersions.exists(v <= _))
          if (validVersions.isEmpty)
            Left(new NoValidScalaVersionFoundError(allVersions))
          else
            Right(validVersions.max.repr)
      }
    }
    val maybeSv = scalaVersion match {
      case None => matchNewestScalaVersion(None)
      case Some(sv0) =>
        if (Util.isFullScalaVersion(sv0)) Right(sv0)
        else matchNewestScalaVersion(Some(sv0))
    }

    val sv  = value(maybeSv)
    val sbv = scalaBinaryVersion.getOrElse(ScalaVersion.binary(sv))
    (sv, sbv)
  }

  lazy val scalaParams: Either[BuildException, ScalaParameters] = either {
    val (scalaVersion, scalaBinaryVersion) =
      value(computeScalaVersions(scalaOptions.scalaVersion, scalaOptions.scalaBinaryVersion))
    val maybePlatformSuffix = platform.value match {
      case Platform.JVM    => None
      case Platform.JS     => Some(scalaJsOptions.platformSuffix)
      case Platform.Native => Some(scalaNativeOptions.platformSuffix)
    }
    ScalaParameters(scalaVersion, scalaBinaryVersion, maybePlatformSuffix)
  }

  def artifacts(logger: Logger): Either[BuildException, Artifacts] = either {
    val maybeArtifacts = Artifacts(
      params = value(scalaParams),
      compilerPlugins = value(compilerPlugins),
      javacPluginDependencies = value(javacPluginDependencies),
      extraJavacPlugins = javaOptions.javacPlugins.map(_.value.toNIO),
      dependencies = value(dependencies),
      extraClassPath = allExtraJars,
      scalaNativeCliVersion =
        if (platform.value == Platform.Native) Some(scalaNativeOptions.finalVersion) else None,
      extraCompileOnlyJars = allExtraCompileOnlyJars,
      extraSourceJars = allExtraSourceJars,
      fetchSources = classPathOptions.fetchSources.getOrElse(false),
      addStubs = internalDependencies.addStubsDependency,
      addJvmRunner = addRunnerDependency,
      addJvmTestRunner = addJvmTestRunner,
      addJsTestBridge = addJsTestBridge,
      addNativeTestInterface = addNativeTestInterface,
      addJmhDependencies = jmhOptions.addJmhDependencies,
      extraRepositories = finalRepositories,
      cache = finalCache,
      logger = logger
    )
    value(maybeArtifacts)
  }

  private def allCrossScalaVersionOptions: Seq[BuildOptions] = {
    val scalaOptions0 = scalaOptions.normalize
    val sortedExtraScalaVersions = scalaOptions0
      .extraScalaVersions
      .toVector
      .map(coursier.core.Version(_))
      .sorted
      .map(_.repr)
      .reverse
    this +: sortedExtraScalaVersions.map { sv =>
      copy(
        scalaOptions = scalaOptions0.copy(
          scalaVersion = Some(sv),
          extraScalaVersions = Set.empty
        )
      )
    }
  }

  private def allCrossScalaPlatformOptions: Seq[BuildOptions] = {
    val scalaOptions0 = scalaOptions.normalize
    val sortedExtraPlatforms = scalaOptions0
      .extraPlatforms
      .toVector
    this +: sortedExtraPlatforms.map { case (pf, pos) =>
      copy(
        scalaOptions = scalaOptions0.copy(
          platform = Some(Positioned(pos.positions, pf)),
          extraPlatforms = Map.empty
        )
      )
    }
  }

  def crossOptions: Seq[BuildOptions] = {
    val allOptions = for {
      svOpt   <- allCrossScalaVersionOptions
      svPfOpt <- svOpt.allCrossScalaPlatformOptions
    } yield svPfOpt
    allOptions.drop(1) // First one if basically 'this', dropping it
  }

  private def clearJsOptions: BuildOptions =
    copy(scalaJsOptions = ScalaJsOptions())
  private def clearNativeOptions: BuildOptions =
    copy(scalaNativeOptions = ScalaNativeOptions())
  private def normalize: BuildOptions = {
    var opt = this

    if (platform.value != Platform.JS)
      opt = opt.clearJsOptions
    if (platform.value != Platform.Native)
      opt = opt.clearNativeOptions

    opt.copy(
      scalaOptions = opt.scalaOptions.normalize
    )
  }

  lazy val hash: Option[String] = {
    val md = MessageDigest.getInstance("SHA-1")

    var hasAnyOverride = false

    BuildOptions.hasHashData.add(
      "",
      normalize,
      s => {
        val bytes = s.getBytes(StandardCharsets.UTF_8)
        if (bytes.length > 0) {
          hasAnyOverride = true
          md.update(bytes)
        }
      }
    )

    if (hasAnyOverride) {
      val digest        = md.digest()
      val calculatedSum = new BigInteger(1, digest)
      val hash          = String.format(s"%040x", calculatedSum).take(10)
      Some(hash)
    }
    else None
  }

  def orElse(other: BuildOptions): BuildOptions =
    BuildOptions.monoid.orElse(this, other)

  def validate: Seq[Diagnostic] = BuildOptionsRule.validateAll(this)
}

object BuildOptions {

  final case class CrossKey(
    scalaVersion: String,
    platform: Platform
  )

  implicit val hasHashData: HasHashData[BuildOptions] = HasHashData.derive
  implicit val monoid: ConfigMonoid[BuildOptions]     = ConfigMonoid.derive
}
