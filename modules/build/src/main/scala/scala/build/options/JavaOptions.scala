package scala.build.options

import dependency.AnyDependency

import scala.build.Positioned
import scala.build.options.collections.StringOptionsList

final case class JavaOptions(
  javaHomeOpt: Option[Positioned[os.Path]] = None,
  jvmIdOpt: Option[String] = None,
  jvmIndexOpt: Option[String] = None,
  jvmIndexOs: Option[String] = None,
  jvmIndexArch: Option[String] = None,
  javaOpts: StringOptionsList = StringOptionsList.empty,
  bloopJvmVersion: Option[Positioned[Int]] = None,
  javacPluginDependencies: Seq[Positioned[AnyDependency]] = Nil,
  javacPlugins: Seq[Positioned[os.Path]] = Nil,
  javacOptions: Seq[String] = Nil
)

object JavaOptions {
  implicit val hasHashData: HasHashData[JavaOptions] = HasHashData.derive
  implicit val monoid: ConfigMonoid[JavaOptions]     = ConfigMonoid.derive
}
