package scala.cli.errors

import scala.build.errors.BuildException

final class MissingPublishOptionError(
  val name: String,
  val optionName: String,
  val directiveName: String,
  val extraMessage: String = ""
) extends BuildException(
      {
        val directivePart =
          if (directiveName.isEmpty)
            s" or with a 'using $directiveName' directive"
          else
            ""
        s"Missing $name for publishing, specify one with $optionName" +
          directivePart +
          extraMessage
      }
    )
