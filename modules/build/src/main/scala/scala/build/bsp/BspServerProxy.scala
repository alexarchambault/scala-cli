package scala.build.bsp

import java.util.concurrent.CompletableFuture

import scala.build.bloop.ScalaDebugServer
import scala.build.{Inputs, Logger}

import ch.epfl.scala.bsp4j as b

class BspServerProxy(
  bloopServer: b.BuildServer & b.ScalaBuildServer & b.JavaBuildServer & ScalaDebugServer,
  compile: (() => CompletableFuture[b.CompileResult]) => CompletableFuture[b.CompileResult],
  logger: Logger,
  initialInputs: Inputs
) extends BspServerWrapper {
  var currentBspServer = new BspServer(bloopServer, compile, logger, initialInputs)
}
