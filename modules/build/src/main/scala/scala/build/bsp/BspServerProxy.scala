package scala.build.bsp

import ch.epfl.scala.bsp4j as b
import com.github.plokhotnyuk.jsoniter_scala.core.{readFromArray, JsonReaderException}

import java.util.concurrent.CompletableFuture

import scala.build.EitherCps.{either, value}
import scala.build.bloop.BloopServer
import scala.build.blooprifle.BloopRifleConfig
import scala.build.compiler.BloopCompiler
import scala.build.internal.Constants
import scala.build.options.BuildOptions
import scala.build.{BloopBuildClient, Build, Inputs, Logger}
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.*

class BspServerProxy(
  bloopRifleConfig: BloopRifleConfig,
  threads: BspThreads,
  localClient: b.BuildClient & BloopBuildClient,
  buildOptions: BuildOptions,
  compile: (() => CompletableFuture[b.CompileResult]) => CompletableFuture[b.CompileResult],
  logger: Logger,
  initialInputs: Inputs,
  argsToInputs: Seq[String] => Either[String, Inputs]
) extends BspServerWrapper {
  var currentBloopCompiler: BloopCompiler = createBloopCompiler(initialInputs)
  localClient.onConnectWithServer(currentBloopCompiler.bloopServer.server)
  var currentBspServer: BspServer = createBspServer(currentBloopCompiler, initialInputs)

  override def workspaceReload(): CompletableFuture[AnyRef] =
    super.workspaceReload().thenCompose { res =>
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
          if (newInputs == previousInputs) CompletableFuture.completedFuture(res)
          else reloadBsp(previousInputs, newInputs)
        }
        thing match {
          case Left(_) =>
            CompletableFuture.completedFuture(res) // TODO return a proper json-rpc error message
          case Right(r) => r
        }
      }
      else
        CompletableFuture.completedFuture(res)
    }

  private def reloadBsp(
    previousInputs: Inputs,
    newInputs: Inputs
  ): CompletableFuture[AnyRef] = {
    val previousTargetIds = currentBspServer.targetIds
    currentBloopCompiler = createBloopCompiler(newInputs)
    currentBspServer = createBspServer(currentBloopCompiler, newInputs)
    val newTargetIds = currentBspServer.targetIds
    if (previousInputs.projectName != newInputs.projectName) {
      val events = newTargetIds.map(buildTargetIdToEvent(_, b.BuildTargetEventKind.CREATED)) ++
        previousTargetIds.map(buildTargetIdToEvent(_, b.BuildTargetEventKind.DELETED))
      val didChangeBuildTargetParams = new b.DidChangeBuildTarget(events.asJava)
      currentBspServer.client.foreach(_.onBuildTargetDidChange(didChangeBuildTargetParams))
    }
    buildTargetCompile(new b.CompileParams(newTargetIds.asJava))
      .thenApply[AnyRef] { _ => // TODO add proper error handling
        new Object()
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

  private def createBspServer(bloopCompiler: BloopCompiler, inputs: Inputs) =
    new BspServer(bloopCompiler.bloopServer.server, compile, logger, inputs)

  private def buildTargetIdToEvent(
    targetId: b.BuildTargetIdentifier,
    eventKind: b.BuildTargetEventKind
  ): b.BuildTargetEvent = {
    val event = new b.BuildTargetEvent(targetId)
    event.setKind(eventKind)
    event
  }
}
