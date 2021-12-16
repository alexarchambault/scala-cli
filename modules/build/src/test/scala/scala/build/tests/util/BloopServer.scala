package scala.build.tests.util

import scala.build.{Bloop, Logger}
import scala.build.blooprifle.BloopRifleConfig

object BloopServer {

  // FIXME We could use a test-specific Bloop instance here.
  // Not sure how to properly shut it down or have it exit after a period
  // of inactivity, so we keep using our default global Bloop for now.
  private def bloopAddress = {
    val directories = scala.build.Directories.default()
    BloopRifleConfig.Address.DomainSocket(
      directories.bloopDaemonDir.toNIO,
      "scalacli\\bloop\\pipe"
    )
  }

  val bloopConfig = BloopRifleConfig.default(
    bloopAddress,
    v => Bloop.bloopClassPath(Logger.nop, v)
  )

}
