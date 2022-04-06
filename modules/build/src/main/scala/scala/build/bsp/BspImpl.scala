package scala.build.bsp

import ch.epfl.scala.{bsp4j => b}
import com.github.plokhotnyuk.jsoniter_scala.core.{readFromArray, JsonReaderException}
import com.swoval.files.PathWatchers
import org.eclipse.lsp4j.jsonrpc
import org.eclipse.lsp4j.jsonrpc.messages.ResponseError

import java.io.{InputStream, OutputStream}
import java.util.concurrent.{CompletableFuture, Executor}

import scala.build.EitherCps.{either, value}
import scala.build._
import scala.build.bloop.{BloopServer, ScalaDebugServer}
import scala.build.blooprifle.BloopRifleConfig
import scala.build.compiler.BloopCompiler
import scala.build.errors.BuildException
import scala.build.internal.{Constants, CustomCodeWrapper}
import scala.build.options.{BuildOptions, Scope}
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success}

final class BspImpl(
  logger: Logger,
  bloopRifleConfig: BloopRifleConfig,
  initialInputs: Inputs,
  argsToInputs: Seq[String] => Either[String, Inputs],
  buildOptions: BuildOptions,
  verbosity: Int,
  threads: BspThreads,
  in: InputStream,
  out: OutputStream
) extends Bsp { self =>
  def notifyBuildChange(): Unit = {
    val events =
      for (targetId <- bspServerProxy.targetIds)
        yield {
          val event = new b.BuildTargetEvent(targetId)
          event.setKind(b.BuildTargetEventKind.CHANGED)
          event
        }
    val params = new b.DidChangeBuildTarget(events.asJava)
    actualLocalClient.onBuildTargetDidChange(params)
  }

  private def prepareBuild(): Either[(BuildException, Scope), PreBuildProject] = either {
    logger.log("Preparing build")

    val persistentLogger  = new PersistentDiagnosticLogger(logger)
    val currentBspServer0 = currentBspServer
    val inputs            = currentBspServer0.inputs

    val crossSources = value {
      CrossSources.forInputs(
        inputs,
        Sources.defaultPreprocessors(
          buildOptions.scriptOptions.codeWrapper.getOrElse(CustomCodeWrapper)
        ),
        persistentLogger
      ).left.map((_, Scope.Main))
    }

    if (verbosity >= 3)
      pprint.err.log(crossSources)

    val scopedSources = value(crossSources.scopedSources(buildOptions))

    if (verbosity >= 3)
      pprint.err.log(scopedSources)

    val sourcesMain = scopedSources.sources(Scope.Main, crossSources.sharedOptions(buildOptions))
    val sourcesTest = scopedSources.sources(Scope.Test, crossSources.sharedOptions(buildOptions))

    if (verbosity >= 3)
      pprint.err.log(sourcesMain)

    val options0Main = sourcesMain.buildOptions
    val options0Test = sourcesTest.buildOptions.orElse(options0Main)

    val generatedSourcesMain =
      sourcesMain.generateSources(inputs.generatedSrcRoot(Scope.Main))
    val generatedSourcesTest =
      sourcesTest.generateSources(inputs.generatedSrcRoot(Scope.Test))

    currentBspServer0.setExtraDependencySources(buildOptions.classPathOptions.extraSourceJars)
    currentBspServer0.setGeneratedSources(Scope.Main, generatedSourcesMain)
    currentBspServer0.setGeneratedSources(Scope.Test, generatedSourcesTest)

    val (classesDir0Main, scalaParamsMain, artifactsMain, projectMain, buildChangedMain) = value {
      val res = Build.prepareBuild(
        inputs,
        sourcesMain,
        generatedSourcesMain,
        options0Main,
        None,
        Scope.Main,
        currentBloopCompiler,
        persistentLogger
      )
      res.left.map((_, Scope.Main))
    }

    val (classesDir0Test, scalaParamsTest, artifactsTest, projectTest, buildChangedTest) = value {
      val res = Build.prepareBuild(
        inputs,
        sourcesTest,
        generatedSourcesTest,
        options0Test,
        None,
        Scope.Test,
        currentBloopCompiler,
        persistentLogger
      )
      res.left.map((_, Scope.Test))
    }

    localClient.setGeneratedSources(Scope.Main, generatedSourcesMain)
    localClient.setGeneratedSources(Scope.Test, generatedSourcesTest)

    val mainScope = PreBuildData(
      sourcesMain,
      options0Main,
      classesDir0Main,
      scalaParamsMain,
      artifactsMain,
      projectMain,
      generatedSourcesMain,
      buildChangedMain
    )

    val testScope = PreBuildData(
      sourcesTest,
      options0Test,
      classesDir0Test,
      scalaParamsTest,
      artifactsTest,
      projectTest,
      generatedSourcesTest,
      buildChangedTest
    )

    PreBuildProject(mainScope, testScope, persistentLogger.diagnostics)
  }

  private def buildE(
    notifyChanges: Boolean
  ): Either[(BuildException, Scope), Unit] = {
    def doBuildOnce(data: PreBuildData, scope: Scope) =
      Build.buildOnce(
        currentBspServer.inputs,
        data.sources,
        data.generatedSources,
        data.buildOptions,
        scope,
        logger,
        actualLocalClient,
        currentBloopCompiler,
        partialOpt = None
      ).left.map(_ -> scope)

    for {
      preBuild <- prepareBuild()
      _ = {
        if (notifyChanges && (preBuild.mainScope.buildChanged || preBuild.testScope.buildChanged))
          notifyBuildChange()
      }
      _ <- doBuildOnce(preBuild.mainScope, Scope.Main)
      _ <- doBuildOnce(preBuild.testScope, Scope.Test)
    } yield ()
  }

  private def build(
    client: BspClient,
    notifyChanges: Boolean,
    logger: Logger
  ): Unit =
    buildE(notifyChanges) match {
      case Left((ex, scope)) =>
        client.reportBuildException(bspServerProxy.targetScopeIdOpt(scope), ex)
        logger.debug(s"Caught $ex during BSP build, ignoring it")
      case Right(()) =>
        for (targetId <- bspServerProxy.targetIds)
          client.resetBuildExceptionDiagnostics(targetId)
    }

  private val shownGlobalMessages =
    new java.util.concurrent.ConcurrentHashMap[String, Unit]()

  private def showGlobalWarningOnce(msg: String) =
    shownGlobalMessages.computeIfAbsent(
      msg,
      _ => {
        val params = new b.ShowMessageParams(b.MessageType.WARNING, msg)
        actualLocalClient.onBuildShowMessage(params)
      }
    )

  def compile(
    executor: Executor,
    doCompile: () => CompletableFuture[b.CompileResult]
  ): CompletableFuture[b.CompileResult] = {
    val preBuild = CompletableFuture.supplyAsync(
      () =>
        prepareBuild() match {
          case Right(preBuild) =>
            if (preBuild.mainScope.buildChanged || preBuild.testScope.buildChanged)
              notifyBuildChange()
            Right(preBuild)
          case Left((ex, scope)) =>
            Left((ex, scope))
        },
      executor
    )

    preBuild.thenCompose {
      case Left((ex, scope)) =>
        actualLocalClient.reportBuildException(bspServerProxy.targetScopeIdOpt(scope), ex)
        CompletableFuture.completedFuture(
          new b.CompileResult(b.StatusCode.ERROR)
        )
      case Right(params) =>
        for (targetId <- bspServerProxy.targetIds)
          actualLocalClient.resetBuildExceptionDiagnostics(targetId)

        val targetId = bspServerProxy.targetIds.head
        params.diagnostics.foreach(actualLocalClient.reportDiagnosticForFiles(targetId))

        doCompile().thenCompose { res =>
          def doPostProcess(data: PreBuildData, scope: Scope): Unit =
            Build.postProcess(
              data.generatedSources,
              currentBspServer.inputs.generatedSrcRoot(scope),
              data.classesDir,
              logger,
              currentBspServer.inputs.workspace,
              updateSemanticDbs = true,
              scalaVersion = data.project.scalaCompiler.scalaVersion
            ).left.foreach(_.foreach(showGlobalWarningOnce))

          if (res.getStatusCode == b.StatusCode.OK)
            CompletableFuture.supplyAsync(
              () => {
                doPostProcess(params.mainScope, Scope.Main)
                doPostProcess(params.testScope, Scope.Test)
                res
              },
              executor
            )
          else
            CompletableFuture.completedFuture(res)
        }
    }
  }

  def registerWatchInputs(watcher: Build.Watcher): Unit =
    currentBspServer.inputs.elements.foreach {
      case elem: Inputs.OnDisk =>
        val eventFilter: PathWatchers.Event => Boolean = { event =>
          val newOrDeletedFile =
            event.getKind == PathWatchers.Event.Kind.Create ||
            event.getKind == PathWatchers.Event.Kind.Delete
          lazy val p        = os.Path(event.getTypedPath.getPath.toAbsolutePath)
          lazy val relPath  = p.relativeTo(elem.path)
          lazy val isHidden = relPath.segments.exists(_.startsWith("."))
          def isScalaFile   = relPath.last.endsWith(".sc") || relPath.last.endsWith(".scala")
          def isJavaFile    = relPath.last.endsWith(".java")
          newOrDeletedFile && !isHidden && (isScalaFile || isJavaFile)
        }
        val watcher0 = watcher.newWatcher()
        watcher0.register(elem.path.toNIO, Int.MaxValue)
        watcher0.addObserver {
          Build.onChangeBufferedObserver { event =>
            if (eventFilter(event))
              watcher.schedule()
          }
        }
      case _ =>
    }

  val actualLocalClient = new BspClient(
    threads.buildThreads.bloop.jsonrpc, // meh
    logger
  )
  actualLocalClient.setProjectName(initialInputs.workspace, initialInputs.projectName, Scope.Main)
  val localClient: b.BuildClient with BloopBuildClient =
    if (verbosity >= 3)
      new BspImpl.LoggingBspClient(actualLocalClient)
    else
      actualLocalClient

  private val bspServerProxy: b.BuildServer with b.ScalaBuildServer with b.JavaBuildServer
    with ScalaDebugServer with ScalaScriptBuildServer with HasGeneratedSources =
    new BspServerWrapper {
      def currentBspServer = self.currentBspServer
      override def workspaceReload(): CompletableFuture[AnyRef] =
        onReload()
    }

  private var currentBloopCompiler = createBloopCompiler(initialInputs)
  localClient.onConnectWithServer(currentBloopCompiler.bloopServer.server)
  private var currentBspServer = createBspServer(initialInputs)

  val watcher = new Build.Watcher(
    ListBuffer(),
    threads.buildThreads.fileWatcher,
    build(actualLocalClient, notifyChanges = true, logger),
    ()
  )

  def run(): Future[Unit] = {

    val localServer
      : b.BuildServer with b.ScalaBuildServer with b.JavaBuildServer with ScalaScriptBuildServer =
      if (verbosity >= 3)
        new LoggingBuildServerAll(bspServerProxy)
      else
        bspServerProxy

    val launcher = new jsonrpc.Launcher.Builder[b.BuildClient]()
      .setExecutorService(threads.buildThreads.bloop.jsonrpc) // FIXME No
      .setInput(in)
      .setOutput(out)
      .setRemoteInterface(classOf[b.BuildClient])
      .setLocalService(localServer)
      .create()
    val remoteClient = launcher.getRemoteProxy
    actualLocalClient.forwardToOpt = Some(remoteClient)
    bspServerProxy.onConnectWithClient(actualLocalClient)

    for (targetId <- currentBspServer.targetIds)
      initialInputs.flattened().foreach {
        case f: Inputs.SingleFile =>
          actualLocalClient.resetDiagnostics(f.path, targetId)
        case _: Inputs.Virtual =>
      }

    prepareBuild() match {
      case Left((ex, scope)) =>
        actualLocalClient.reportBuildException(bspServerProxy.targetScopeIdOpt(scope), ex)
        logger.log(ex)
      case Right(_) =>
    }

    logger.log {
      val hasConsole = System.console() != null
      if (hasConsole)
        "Listening to incoming JSONRPC BSP requests, press Ctrl+D to exit."
      else
        "Listening to incoming JSONRPC BSP requests."
    }
    val f = launcher.startListening()

    val initiateFirstBuild: Runnable = { () =>
      try build(actualLocalClient, notifyChanges = false, logger)
      catch {
        case t: Throwable =>
          logger.debug(s"Caught $t during initial BSP build, ignoring it")
      }
    }
    threads.prepareBuildExecutor.submit(initiateFirstBuild)

    registerWatchInputs(watcher)

    val es = ExecutionContext.fromExecutorService(threads.buildThreads.bloop.jsonrpc)
    val futures = Seq(
      BspImpl.naiveJavaFutureToScalaFuture(f).map(_ => ())(es),
      currentBspServer.initiateShutdown
    )
    Future.firstCompletedOf(futures)(es)
  }

  def shutdown(): Unit = {
    watcher.dispose()
    if (currentBloopCompiler != null)
      currentBloopCompiler.shutdown()
  }

  private def reloadBsp(
    previousInputs: Inputs,
    newInputs: Inputs
  ): CompletableFuture[AnyRef] = {
    val previousTargetIds = currentBspServer.targetIds
    currentBloopCompiler = createBloopCompiler(newInputs)
    currentBspServer = createBspServer(newInputs)
    val newTargetIds = currentBspServer.targetIds
    prepareBuild() match {
      case Left((buildException, scope)) =>
        CompletableFuture.completedFuture(
          new ResponseError(
            JsonRpcErrorCodes.InternalError,
            s"Can't reload workspace, build failed for scope ${scope.name}: ${buildException.message}",
            new Object()
          )
        )
      case Right(preBuildProject) =>
        if (previousInputs.projectName != preBuildProject.mainScope.project.projectName) {
          val events = newTargetIds.map(BspImpl.buildTargetIdToEvent(_, b.BuildTargetEventKind.CREATED)) ++
            previousTargetIds.map(BspImpl.buildTargetIdToEvent(_, b.BuildTargetEventKind.DELETED))
          val didChangeBuildTargetParams = new b.DidChangeBuildTarget(events.asJava)
          currentBspServer.client.foreach(_.onBuildTargetDidChange(didChangeBuildTargetParams))
        }
        CompletableFuture.completedFuture(new Object())
    }
  }

  private def createBloopCompiler(inputs: Inputs): BloopCompiler = {
    val bloopServer = BloopServer.buildServer(
      bloopRifleConfig,
      "scala-cli",
      Constants.version,
      (inputs.workspace / Constants.workspaceDirName).toNIO,
      Build.classesRootDir(inputs.workspace, inputs.projectName).toNIO,
      localClient,
      threads.buildThreads.bloop,
      logger.bloopRifleLogger
    )
    new BloopCompiler(
      bloopServer,
      20.seconds,
      strictBloopJsonCheck = buildOptions.internal.strictBloopJsonCheckOrDefault
    )
  }

  private def createBspServer(inputs: Inputs) =
    new BspServer(
      currentBloopCompiler.bloopServer.server,
      doCompile => compile(threads.prepareBuildExecutor, doCompile),
      logger,
      inputs
    )

  private def onReload(): CompletableFuture[AnyRef] = {
    val ideInputsJsonPath =
      currentBspServer.workspace / Constants.workspaceDirName / "ide-inputs.json"
    if (os.isFile(ideInputsJsonPath)) {
      val thing = either[String] {
        val ideInputs = value {
          try Right(readFromArray(os.read.bytes(ideInputsJsonPath))(IdeInputs.codec))
          catch {
            case e: JsonReaderException =>
              logger.debug(s"Caught $e while decoding $ideInputsJsonPath")
              Left(e.getMessage)
          }
        }
        val newInputs = value(argsToInputs(ideInputs.args))
        val previousInputs = currentBspServer.inputs
        if (newInputs == previousInputs) CompletableFuture.completedFuture(new Object)
        else reloadBsp(previousInputs, newInputs)
      }
      thing match {
        case Left(errorMessage) =>
          CompletableFuture.completedFuture(
            BspImpl.responseError(s"Workspace reload failed, couldn't load sources: $errorMessage")
          )
        case Right(r) => r
      }
    }
    else
      CompletableFuture.completedFuture(
        BspImpl.responseError(
          s"Workspace reload failed, inputs file missing from workspace directory: ${ideInputsJsonPath.toString()}"
        )
      )
  }
}

object BspImpl {

  private def buildTargetIdToEvent(
    targetId: b.BuildTargetIdentifier,
    eventKind: b.BuildTargetEventKind
  ): b.BuildTargetEvent = {
    val event = new b.BuildTargetEvent(targetId)
    event.setKind(eventKind)
    event
  }

  private def responseError(
    message: String,
    errorCode: Int = JsonRpcErrorCodes.InternalError
  ): ResponseError =
    new ResponseError(errorCode, message, new Object())

  // from https://github.com/com-lihaoyi/Ammonite/blob/7eb58c58ec8c252dc5bd1591b041fcae01cccf90/amm/interp/src/main/scala/ammonite/interp/script/AmmoniteBuildServer.scala#L550-L565
  private def naiveJavaFutureToScalaFuture[T](
    f: java.util.concurrent.Future[T]
  ): Future[T] = {
    val p = Promise[T]()
    val t = new Thread {
      setDaemon(true)
      setName("bsp-wait-for-exit")
      override def run(): Unit =
        p.complete {
          try Success(f.get())
          catch { case t: Throwable => Failure(t) }
        }
    }
    t.start()
    p.future
  }

  private final class LoggingBspClient(actualLocalClient: BspClient) extends LoggingBuildClient
      with BloopBuildClient {
    // in Scala 3 type of the method needs to be explicitly overridden
    def underlying: scala.build.bsp.BspClient = actualLocalClient
    def clear()                               = underlying.clear()
    def diagnostics                           = underlying.diagnostics
    def setProjectParams(newParams: Seq[String]) =
      underlying.setProjectParams(newParams)
    def setGeneratedSources(scope: Scope, newGeneratedSources: Seq[GeneratedSource]) =
      underlying.setGeneratedSources(scope, newGeneratedSources)
  }
}
