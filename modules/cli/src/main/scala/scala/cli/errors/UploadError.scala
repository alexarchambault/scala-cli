package scala.cli.errors

import coursier.publish.fileset.Path
import coursier.publish.upload.Upload
import coursier.publish.Content

import scala.build.errors.BuildException

final class UploadError(errors: ::[(Path, Content, Upload.Error)]) extends BuildException(
      s"Error uploading ${errors.length} file(s):" +
        errors
          .map {
            case (path, _, err) =>
              System.lineSeparator() + s"  ${path.repr}: ${err.getMessage}"
          }
          .mkString
    )
