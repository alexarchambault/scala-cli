package scala.build.bsp

import ch.epfl.scala.bsp4j as b
import com.swoval.files.PathWatchers

import java.util.concurrent.CompletableFuture
import java.util.concurrent.atomic.AtomicReference

import scala.build.bloop.BloopServer
import scala.build.blooprifle.BloopRifleConfig
import scala.build.compiler.BloopCompiler
import scala.build.internal.Constants
import scala.build.options.BuildOptions
import scala.build.{Build, Inputs, Logger}
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.DurationInt

final class BloopSession(
  val inputs: Inputs,
  val remoteServer: BloopCompiler,
  val bspServer: BspServer,
  val watcher: Build.Watcher
) {
  def resetDiagnostics(localClient: BspClient): Unit =
    for (targetId <- bspServer.targetIds)
      inputs.flattened().foreach {
        case f: Inputs.SingleFile =>
          localClient.resetDiagnostics(f.path, targetId)
        case _: Inputs.Virtual =>
      }
  def dispose(): Unit = {
    watcher.dispose()
    remoteServer.shutdown()
  }

  def registerWatchInputs(): Unit =
    inputs.elements.foreach {
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
}

object BloopSession {

  def create(
    inputs: Inputs,
    bloopRifleConfig: BloopRifleConfig,
    threads: BspThreads,
    buildOptions: BuildOptions,
    buildClient: b.BuildClient,
    logger: Logger,
    compile: (() => CompletableFuture[b.CompileResult]) => BloopSession => CompletableFuture[b.CompileResult],
    build: BloopSession => Unit
  ): BloopSession = {
    val bloopServer = BloopServer.buildServer(
      bloopRifleConfig,
      "scala-cli",
      Constants.version,
      (inputs.workspace / Constants.workspaceDirName).toNIO,
      Build.classesRootDir(inputs.workspace, inputs.projectName).toNIO,
      buildClient,
      threads.buildThreads.bloop,
      logger.bloopRifleLogger
    )
    val remoteServer = new BloopCompiler(
      bloopServer,
      20.seconds,
      strictBloopJsonCheck = buildOptions.internal.strictBloopJsonCheckOrDefault
    )
    lazy val bspServer = new BspServer(
      remoteServer.bloopServer.server,
      doCompile => compile(doCompile)(bloopSession0),
      logger
    )

    lazy val watcher = new Build.Watcher(
      ListBuffer(),
      threads.buildThreads.fileWatcher,
      build(bloopSession0),
      ()
    )
    lazy val bloopSession0: BloopSession = new BloopSession(
      inputs,
      remoteServer,
      bspServer,
      watcher
    )

    bloopSession0.registerWatchInputs()
    bspServer.newInputs(inputs)

    bloopSession0
  }

  final class Reference {
    private val ref = new AtomicReference[BloopSession](null)
    def get(): BloopSession = {
      val session = ref.get()
      if (session == null)
        sys.error("BSP server not initialized yet")
      session
    }
    def getAndNullify(): Option[BloopSession] =
      Option(ref.getAndSet(null))
    def update(former: BloopSession, newer: BloopSession, ifError: String): Unit =
      if (!ref.compareAndSet(former, newer))
        sys.error(ifError)
  }
}
