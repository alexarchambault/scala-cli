package scala.build.options.publish

import scala.build.Positioned
import scala.build.errors.{BuildException, MalformedInputError}

final case class Vcs(
  url: String,
  connection: String,
  developerConnection: String
)

object Vcs {

  def gitHub(org: String, name: String): Vcs =
    Vcs(
      s"https://github.com/$org/$name",
      s"scm:git:git://github.com/$org/$name.git",
      s"scm:git:ssh://git@github.com:$org/$name.git"
    )

  def parse(input: Positioned[String]): Either[BuildException, Vcs] =
    if (input.value.startsWith("github:"))
      input.value.stripPrefix("github:").split("/", 2) match {
        case Array(org, project) =>
          Right(Vcs.gitHub(org, project))
        case _ =>
          Left(
            new MalformedInputError(
              "github-vcs",
              input.value,
              "github:org/project",
              input.positions
            )
          )
      }
    else
      input.value.split("|", 3) match {
        case Array(url, conn, devConn) =>
          val vcs = Vcs(url, conn, devConn)
          Right(vcs)
        case _ =>
          Left(
            new MalformedInputError(
              "vcs",
              input.value,
              "url|connection|developer-connection",
              input.positions
            )
          )
      }
}
