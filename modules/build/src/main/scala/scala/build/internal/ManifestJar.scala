package scala.build.internal

import java.io.OutputStream

object ManifestJar {

  def create(
    classPath: Seq[os.Path],
    scratchDirOpt: Option[os.Path] = None
  ): os.Path = {
    import java.util.jar._
    val manifest   = new Manifest
    val attributes = manifest.getMainAttributes
    attributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")
    attributes.put(Attributes.Name.CLASS_PATH, classPath.map(_.toString).mkString(" "))
    val jarFile = scratchDirOpt match {
      case Some(scratchDir) =>
        os.makeDir.all(scratchDir)
        os.temp(dir = scratchDir, prefix = "classpathJar", suffix = ".jar", deleteOnExit = false)
      case None =>
        os.temp(prefix = "classpathJar", suffix = ".jar")
    }
    var os0: OutputStream    = null
    var jos: JarOutputStream = null
    try {
      os0 = os.write.outputStream(jarFile)
      jos = new JarOutputStream(os0, manifest)
    }
    finally {
      if (jos != null)
        jos.close()
      if (os0 != null)
        os0.close()
    }
    jarFile
  }

  def maybeWithManifestClassPath[T](
    createManifest: Boolean,
    classPath: Seq[os.Path]
  )(
    f: Seq[os.Path] => T
  ): T = {
    if (createManifest) {
      var toDeleteOpt = Option.empty[os.Path]

      try {
        val manifestJar = create(classPath)
        toDeleteOpt = Some(manifestJar)
        f(Seq(manifestJar))
      }
      finally
        for (toDelete <- toDeleteOpt)
          os.remove(toDelete)
    }
    else
      f(classPath)
  }

}
