package scala.cli.config

import scala.build.errors.BuildException

final class ConfigDbException(cause: Exception)
    extends BuildException(s"Config DB error: ${cause.getMessage}", cause = cause)
