package scala.build.options

final case class BuildRequirements(
  scalaVersion: Seq[BuildRequirements.VersionRequirement] = Nil
)

object BuildRequirements {

  sealed trait VersionRequirement extends Product with Serializable {
    def valid(version: String): Boolean
    def failedMessage: String
  }

  final case class VersionEquals(requiredVersion: String) extends VersionRequirement {
    def valid(version: String): Boolean = {
      val cmp = coursier.core.Version(requiredVersion).compare(coursier.core.Version(version))
      cmp == 0
    }
    def failedMessage: String = s"Expected version $requiredVersion"
  }
  final case class VersionLowerThan(maxVersion: String, orEqual: Boolean) extends VersionRequirement {
    def valid(version: String): Boolean = {
      val cmp = coursier.core.Version(version).compare(coursier.core.Version(maxVersion))
      cmp < 0 || (orEqual && cmp == 0)
    }
    def failedMessage: String =
      if (orEqual) s"Expected version lower than or equal to $maxVersion"
      else s"Expected version lower than $maxVersion"
  }
  final case class VersionHigherThan(minVersion: String, orEqual: Boolean) extends VersionRequirement {
    def valid(version: String): Boolean = {
      val cmp = coursier.core.Version(minVersion).compare(coursier.core.Version(version))
      cmp < 0 || (orEqual && cmp == 0)
    }
    def failedMessage: String =
      if (orEqual) s"Expected version higher than or equal to $minVersion"
      else s"Expected version higher than $minVersion"
  }

  implicit val monoid: ConfigMonoid[BuildRequirements] = ConfigMonoid.derive

}
