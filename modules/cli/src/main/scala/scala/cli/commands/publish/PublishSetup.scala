package scala.cli.commands.publish

import caseapp.core.RemainingArgs
import coursier.cache.ArchiveCache

import java.nio.charset.StandardCharsets

import scala.build.Ops._
import scala.build.errors.CompositeBuildException
import scala.build.internal.CustomCodeWrapper
import scala.build.options.{BuildOptions, InternalOptions, Scope}
import scala.build.{CrossSources, Sources}
import scala.cli.commands.ScalaCommand
import scala.cli.commands.github.{LibSodiumJni, SecretCreate, SecretList}
import scala.cli.commands.util.CommonOps._
import scala.cli.commands.util.{ScalaCliSttpBackend, SharedOptionsUtil}
import scala.cli.config.{ConfigDb, Entries}
import scala.cli.internal.Constants

object PublishSetup extends ScalaCommand[PublishSetupOptions] {

  override def group      = "Main"
  override def inSipScala = false

  override def names = List(
    List("publish", "setup")
  )

  def run(options: PublishSetupOptions, args: RemainingArgs): Unit = {

    val logger        = options.logging.logger
    val coursierCache = options.coursier.coursierCache(logger.coursierLogger(""))

    lazy val configDb = ConfigDb.open(options.directories.directories)
      .orExit(logger)

    val inputs = {
      val maybeInputs = SharedOptionsUtil.inputs(
        args.all,
        () => None,
        Nil,
        options.directories.directories,
        logger,
        coursierCache,
        None,
        options.input.defaultForbiddenDirectories,
        options.input.forbid
      )
      maybeInputs match {
        case Left(error) =>
          System.err.println(error)
          sys.exit(1)
        case Right(inputs0) => inputs0
      }
    }

    val (pureJava, publishOptions) = {
      val cliBuildOptions = BuildOptions(
        internal = InternalOptions(
          cache = Some(coursierCache)
        )
      )

      val crossSources = CrossSources.forInputs(
        inputs,
        Sources.defaultPreprocessors(
          cliBuildOptions.scriptOptions.codeWrapper.getOrElse(CustomCodeWrapper)
        ),
        logger
      ).orExit(logger)

      val crossSourcesSharedOptions = crossSources.sharedOptions(cliBuildOptions)
      val scopedSources = crossSources.scopedSources(crossSourcesSharedOptions).orExit(logger)
      val sources       = scopedSources.sources(Scope.Main, crossSourcesSharedOptions)

      val pureJava = sources.hasJava && !sources.hasScala

      (pureJava, sources.buildOptions.notForBloopOptions.publishOptions)
    }

    val backend = ScalaCliSttpBackend.httpURLConnection(logger)

    val checksInputOpt = options.checks.map(_.trim).filter(_.nonEmpty).filter(_ != "all")
    val checkKinds = checksInputOpt match {
      case None => OptionCheck.Kind.all.toSet
      case Some(checksInput) =>
        OptionCheck.Kind.parseList(checksInput)
          .left.map { _ =>
            ???
          }
          .orExit(logger)
          .toSet
    }

    val missingFields =
      OptionChecks.checks(options, configDb, inputs.workspace, coursierCache, logger, backend)
        .filter(check => checkKinds(check.kind))
        .flatMap {
          check =>
            if (check.check(publishOptions)) {
              logger.debug(s"Found field ${check.fieldName}")
              Nil
            }
            else {
              logger.debug(s"Missing field ${check.fieldName}")
              Seq(check)
            }
        }

    if (missingFields.nonEmpty) {
      System.err.println(s"${missingFields.length} option(s) need to be set:")
      for (check <- missingFields)
        System.err.println(s"- ${check.fieldName}")
    }

    lazy val (ghRepoOrg, ghRepoName) = GitRepo.ghRepoOrgName(inputs.workspace, logger)
      .orExit(logger)

    lazy val token = options.token
      .orElse(configDb.get(Entries.ghToken).orExit(logger))
      .map(_.get())
      .getOrElse {
        System.err.println(
          s"No GitHub token passed, please specify one via --token env:ENV_VAR_NAME or --token file:/path/to/token, " +
            s"or by setting ${Entries.ghToken.fullName} in the config."
        )
        sys.exit(1)
      }

    if (options.check)
      if (missingFields.isEmpty)
        logger.message("Setup fine for publishing")
      else {
        logger.message("Found missing config for publishing")
        sys.exit(1)
      }
    else {

      val missingFieldsWithDefaults = missingFields
        .map { check =>
          check.defaultValue().map((check, _))
        }
        .sequence
        .left.map(CompositeBuildException(_))
        .orExit(logger)

      lazy val secretNames = {
        val secretList = SecretList.list(
          ghRepoOrg,
          ghRepoName,
          token,
          backend,
          logger
        ).orExit(logger)

        secretList.secrets.map(_.name).toSet
      }

      val missingSetSecrets = missingFieldsWithDefaults
        .flatMap {
          case (_, default) => default.ghSecrets
        }
        .filter(s => s.force || !secretNames.contains(s.name))

      if (missingSetSecrets.nonEmpty) {

        LibSodiumJni.init(coursierCache, ArchiveCache().withCache(coursierCache), logger)

        val pubKey = SecretCreate.publicKey(
          ghRepoOrg,
          ghRepoName,
          token,
          backend,
          logger
        ).orExit(logger)

        missingSetSecrets
          .map { s =>
            SecretCreate.createOrUpdate(
              ghRepoOrg,
              ghRepoName,
              token,
              s.name,
              s.value,
              pubKey,
              dummy = false,
              printRequest = false,
              backend,
              logger
            )
          }
          .sequence
          .left.map(CompositeBuildException(_))
          .orExit(logger)
      }

      if (missingFieldsWithDefaults.nonEmpty) {

        val missingFieldsWithDefaultsAndValues = missingFieldsWithDefaults
          .map {
            case (check, default) =>
              default.getValue().map(v => (check, default, v))
          }
          .sequence
          .left.map(CompositeBuildException(_))
          .orExit(logger)

        val dest = {
          val ext = if (pureJava) ".java" else ".scala"
          inputs.workspace / s"publish-conf$ext"
        }
        val nl = System.lineSeparator() // FIXME Get from dest if it exists?
        val extraLines = missingFieldsWithDefaultsAndValues.map {
          case (check, default, value) =>
            s"""//> using ${check.directivePath} "$value"""" + nl +
              default.extraDirectives
                .map {
                  case (k, v) =>
                    s"""//> using $k "$v"""" + nl
                }
                .mkString
        }

        val currentContent =
          if (os.isFile(dest)) os.read.bytes(dest)
          else if (os.exists(dest)) sys.error(s"Error: $dest already exists and is not a file")
          else Array.emptyByteArray
        val updatedContent = currentContent ++
          extraLines.toArray.flatMap(_.getBytes(StandardCharsets.UTF_8))
        os.write.over(dest, updatedContent)
        logger.message(s"Wrote $dest")
      }

      if (options.checkWorkflow.getOrElse(true)) {
        val workflowDir = inputs.workspace / ".github" / "workflows"
        val hasWorkflows = os.isDir(workflowDir) &&
          os.list(workflowDir)
            .filter(_.last.endsWith(".yml")) // FIXME Accept more extensions?
            .filter(os.isFile(_))
            .nonEmpty
        if (hasWorkflows)
          logger.message(
            s"Found some workflow files under $workflowDir, not writing Scala CLI workflow"
          )
        else {
          val dest = workflowDir / "ci.yml"
          val content = {
            val resourcePath = Constants.defaultFilesResourcePath + "/workflows/default.yml"
            val cl           = Thread.currentThread().getContextClassLoader
            val resUrl       = cl.getResource(resourcePath)
            if (resUrl == null)
              sys.error(s"Should not happen - resource $resourcePath not found")
            val is = resUrl.openStream()
            try is.readAllBytes()
            finally is.close()
          }
          os.write(dest, content, createFolders = true)
          logger.message(s"Wrote workflow in $dest")
        }
      }

      if (missingFields.isEmpty)
        logger.message("Setup fine for publishing")
    }
  }
}
