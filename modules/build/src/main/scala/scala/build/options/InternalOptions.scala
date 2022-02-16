package scala.build.options

import coursier.cache.FileCache
import coursier.util.Task

final case class InternalOptions(
  keepDiagnostics: Boolean = false,
  cache: Option[FileCache[Task]] = None,
  localRepository: Option[String] = None,
  verbosity: Option[Int] = None
)

object InternalOptions {
  implicit val monoid: ConfigMonoid[InternalOptions]     = ConfigMonoid.derive
}
