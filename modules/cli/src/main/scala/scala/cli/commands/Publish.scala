package scala.cli.commands

import caseapp.core.RemainingArgs
import coursier.core.Configuration
import coursier.maven.MavenRepository
import coursier.publish.checksum.logger.InteractiveChecksumLogger
import coursier.publish.checksum.{Checksums, ChecksumType}
import coursier.publish.fileset.{FileSet, Path}
import coursier.publish.signing.logger.InteractiveSignerLogger
import coursier.publish.signing.{GpgSigner, NopSigner, Signer}
import coursier.publish.upload.logger.InteractiveUploadLogger
import coursier.publish.upload.{FileUpload, HttpURLConnectionUpload}
import coursier.publish.{Content, Pom}

import java.io.OutputStreamWriter
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.Paths
import java.time.Instant

import scala.build.{Build, Builds, Logger, Os}
import scala.build.EitherCps.{either, value}
import scala.build.Ops._
import scala.build.errors.{BuildException, CompositeBuildException}
import scala.build.options.{ConfigMonoid, Scope}
import scala.cli.CurrentParams
import scala.cli.errors.{FailedToSignFileError, MissingRepositoryError, UploadError}

object Publish extends ScalaCommand[PublishOptions] {

  override def group      = "Main"
  override def inSipScala = false
  override def sharedOptions(options: PublishOptions) =
    Some(options.shared)

  def run(options: PublishOptions, args: RemainingArgs): Unit = {
    maybePrintGroupHelp(options)
    CurrentParams.verbosity = options.shared.logging.verbosity
    val inputs = options.shared.inputsOrExit(args)
    CurrentParams.workspaceOpt = Some(inputs.workspace)

    val logger              = options.shared.logger
    val initialBuildOptions = options.buildOptions.orExit(logger)
    val bloopRifleConfig    = options.shared.bloopRifleConfig()

    val cross = options.compileCross.cross.getOrElse(false)

    lazy val workingDir = options.workingDir
      .filter(_.trim.nonEmpty)
      .map(os.Path(_, Os.pwd))
      .getOrElse {
        os.temp.dir(
          prefix = "scala-cli-publish-",
          deleteOnExit = true
        )
      }

    if (options.watch.watch) {
      val watcher = Build.watch(
        inputs,
        initialBuildOptions,
        bloopRifleConfig,
        logger,
        crossBuilds = cross,
        partial = None,
        postAction = () => WatchUtil.printWatchMessage()
      ) { res =>
        res.orReport(logger).foreach { builds =>
          maybePublish(builds, workingDir, logger, allowExit = false)
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
          bloopRifleConfig,
          logger,
          crossBuilds = cross,
          partial = None
        ).orExit(logger)
      maybePublish(builds, workingDir, logger, allowExit = true)
    }
  }

  def defaultOrganization: Either[BuildException, String] =
    Right("default")
  def defaultName: Either[BuildException, String] =
    Right("default")
  def defaultVersion: Either[BuildException, String] =
    Right("0.1.0-SNAPSHOT")

  private def maybePublish(
    builds: Builds,
    workingDir: os.Path,
    logger: Logger,
    allowExit: Boolean
  ): Unit = {

    val allOk = builds.all.forall {
      case _: Build.Successful => true
      case _: Build.Failed     => false
    }
    if (allOk) {
      val builds0 = builds.all.collect {
        case s: Build.Successful => s
      }
      val res = doPublish(builds0, workingDir, logger)
      if (allowExit)
        res.orExit(logger)
      else
        res.orReport(logger)
    }
    else {
      System.err.println("Compilation failed")
      if (allowExit)
        sys.exit(1)
    }
  }

  private def buildFileSet(
    build: Build.Successful,
    workingDir: os.Path,
    now: Instant,
    logger: Logger
  ): Either[BuildException, FileSet] = either {

    logger.debug(s"Preparing project ${build.project.projectName}")

    val publishOptions = build.options.notForBloopOptions.publishOptions

    val org = publishOptions.organization match {
      case Some(org0) => org0.value
      case None       => value(defaultOrganization)
    }
    val name = publishOptions.name match {
      case Some(name0) => name0.value
      case None        => value(defaultName)
    }
    val ver = publishOptions.version match {
      case Some(ver0) => ver0.value
      case None       => value(defaultVersion)
    }

    val dependencies = build.artifacts.detailedArtifacts.map {
      case (dep, _, _, _) =>
        val config =
          if (build.scope == Scope.Main) None
          else Some(Configuration(build.scope.name))
        (dep.module.organization, dep.module.name, dep.version, config)
    }

    val fullName = {
      val params = build.artifacts.params
      val pf = publishOptions.scalaPlatformSuffix.getOrElse {
        // FIXME Allow full cross version too
        "_" + params.scalaBinaryVersion
      }
      val sv = publishOptions.scalaVersionSuffix.getOrElse {
        params.platform.fold("")("_" + _)
      }
      name + pf + sv
    }

    val mainJarContent = Package.libraryJar(build)
    val mainJar        = workingDir / org / s"$fullName-$ver.jar"
    os.write(mainJar, mainJarContent, createFolders = true)

    val pomContent = Pom.create(
      organization = coursier.Organization(org),
      moduleName = coursier.ModuleName(fullName),
      version = ver,
      packaging = None,
      url = publishOptions.url.map(_.value),
      name = Some(name), // ?
      dependencies = dependencies,
      description = publishOptions.description,
      license = publishOptions.license.map(_.value).map { l =>
        Pom.License(l.name, l.url)
      },
      scm = publishOptions.versionControl.map { vcs =>
        Pom.Scm(vcs.url, vcs.connection, vcs.developerConnection)
      },
      developers = publishOptions.developers.map { dev =>
        Pom.Developer(dev.id, dev.name, dev.url, dev.mail)
      }
    )

    val basePath = Path(org.split('.').toSeq ++ Seq(fullName, ver))

    // TODO Signing, version listings, checksums, …
    FileSet(Seq(
      (basePath / s"$fullName-$ver.pom") -> Content.InMemory(
        now,
        pomContent.getBytes(StandardCharsets.UTF_8)
      ),
      (basePath / s"$fullName-$ver.jar") -> Content.File(mainJar.toNIO)
    ))
  }

  private def doPublish(
    builds: Seq[Build.Successful],
    workingDir: os.Path,
    logger: Logger
  ): Either[BuildException, Unit] = either {

    val now = Instant.now()
    val fileSet0 = value {
      builds
        // TODO Allow to add test JARs to the main build artifacts
        .filter(_.scope != Scope.Test)
        .map { build =>
          buildFileSet(build, workingDir, now, logger)
        }
        .sequence
        .left.map(CompositeBuildException(_))
        .map(_.foldLeft(FileSet.empty)(_ ++ _))
    }

    val ec = builds.head.options.finalCache.ec

    val gpgSignatureIdOpt =
      ConfigMonoid[Option[String]].sum(builds.map(_.options.notForBloopOptions.publishOptions.gpgSignatureId))
    val signer: Signer = gpgSignatureIdOpt match {
      case Some(gpgSignatureId) =>
        GpgSigner(GpgSigner.Key.Id(gpgSignatureId))
      case None =>
        NopSigner
    }
    val signerLogger =
      new InteractiveSignerLogger(new OutputStreamWriter(System.err), verbosity = 1)
    val signRes = signer.signatures(
      fileSet0,
      now,
      ChecksumType.all.map(_.extension).toSet,
      Set("maven-metadata.xml"),
      signerLogger
    )

    val fileSet1 = value {
      signRes
        .left.map {
          case (path, content, err) =>
            val path0 = content.pathOpt
              .map(os.Path(_, Os.pwd))
              .toRight(path.repr)
            new FailedToSignFileError(path0, err)
        }
        .map { signatures =>
          fileSet0 ++ signatures
        }
    }

    val checksumLogger =
      new InteractiveChecksumLogger(new OutputStreamWriter(System.err), verbosity = 1)
    val checksums = Checksums(
      Seq(ChecksumType.MD5, ChecksumType.SHA1),
      fileSet1,
      now,
      ec,
      checksumLogger
    ).unsafeRun()(ec)
    val fileSet2 = fileSet1 ++ checksums

    val finalFileSet = fileSet2.order(ec).unsafeRun()(ec)

    val repoUrl = builds.head.options.notForBloopOptions.publishOptions.repository match {
      case None =>
        value(Left(new MissingRepositoryError))
      case Some(repo) =>
        if (repo.contains("://")) repo
        else os.Path(repo, Os.pwd).toNIO.toUri.toASCIIString
    }
    val repo = MavenRepository(repoUrl)

    val upload =
      if (repo.root.startsWith("http")) HttpURLConnectionUpload.create()
      else FileUpload(Paths.get(new URI(repo.root)))

    val dummy        = false
    val isLocal      = true
    val uploadLogger = InteractiveUploadLogger.create(System.err, dummy = dummy, isLocal = isLocal)

    val errors =
      upload.uploadFileSet(repo, finalFileSet, uploadLogger, Some(ec)).unsafeRun()(ec)

    errors.toList match {
      case h :: t =>
        value(Left(new UploadError(::(h, t))))
      case Nil =>
    }
  }
}
