package sclicheck

class GifTests extends munit.FunSuite {

  val scenariosDir =
    Option(System.getenv("SCALA_CLI_GIF_SCENARIOS")).map(os.Path(_, os.pwd)).getOrElse {
      sys.error("SCALA_CLI_GIF_SCENARIOS not set")
    }
  val websiteImgDir =
    Option(System.getenv("SCALA_CLI_WEBSITE_IMG")).map(os.Path(_, os.pwd)).getOrElse {
      sys.error("SCALA_CLI_WEBSITE_IMG not set")
    }
  lazy val gifRenderedDockerDir =
    Option(System.getenv("SCALA_CLI_GIF_RENDERER_DOCKER_DIR")).map(os.Path(_, os.pwd)).getOrElse {
      sys.error("SCALA_CLI_GIF_RENDERER_DOCKER_DIR not set")
    }
  lazy val svgRenderedDockerDir =
    Option(System.getenv("SCALA_CLI_SVG_RENDERER_DOCKER_DIR")).map(os.Path(_, os.pwd)).getOrElse {
      sys.error("SCALA_CLI_SVG_RENDERER_DOCKER_DIR not set")
    }

  val scenarioScripts = os.list(scenariosDir)
    .filter(!_.last.startsWith("."))
    .filter(_.last.endsWith(".sh"))
    .filter(os.isFile(_))

  lazy val hasTty =
    os.proc("tty").call(stdin = os.Inherit, stdout = os.Inherit, check = false).exitCode == 0
  lazy val ttyOpts = if (hasTty) Seq("-it") else Nil

  def buildImages = true

  def columns = 70
  def rows    = 20
  def record  = true
  def gifs    = true
  def svgs    = true

  def maybeBuildImages(): Unit =
    if (buildImages) {
      os.proc("docker", "build", gifRenderedDockerDir, "--tag", "gif-renderer")
        .call(stdin = os.Inherit, stdout = os.Inherit)
      os.proc("docker", "build", svgRenderedDockerDir, "--tag", "svg_rendrer")
        .call(stdin = os.Inherit, stdout = os.Inherit)
    }

  for (script <- scenarioScripts) {
    val name = script.last.stripSuffix(".sh")
    test(name) {
      maybeBuildImages()

      val svgRenderMappings =
        Seq("-v", s"$websiteImgDir:/data", "-v", "$OUT/.scala:/out")
      val svgRenderOps = Seq(
        "--in",
        s"/out/$name.cast",
        "--width",
        columns.toString,
        "--height",
        rows.toString,
        "--term",
        "iterm2",
        "--padding",
        "20"
      )

      if (record)
        os.proc(
          "docker",
          "run",
          "--rm",
          ttyOpts,
          "-v",
          "$OUT/.scala:/data/out",
          "gif-renderer",
          "./run_scenario.sh",
          name
        )
          .call(stdin = os.Inherit, stdout = os.Inherit)

      if (hasTty) {
        if (svgs) {
          os.proc(
            "docker",
            "run",
            "--rm",
            svgRenderMappings,
            "svg_rendrer",
            "a",
            svgRenderOps,
            "--out",
            s"/data/$name.svg",
            "--profile",
            "/profiles/light"
          )
            .call(stdin = os.Inherit, stdout = os.Inherit)
          os.proc(
            "docker",
            "run",
            "--rm",
            svgRenderMappings,
            "svg_rendrer",
            "a",
            svgRenderOps,
            "--out",
            s"/data/dark/$name.svg",
            "--profile",
            "/profiles/dark"
          )
            .call(stdin = os.Inherit, stdout = os.Inherit)
        }
        if (gifs) {
          os.proc(
            "docker",
            "run",
            "--rm",
            svgRenderMappings,
            "asciinema/asciicast2gif",
            "-w",
            columns,
            "-h",
            rows,
            "-t",
            "monokai",
            s"/out/$name.cast",
            s"/data/gifs/$name.gif"
          )
            .call(stdin = os.Inherit, stdout = os.Inherit)
          os.proc(
            "docker",
            "run",
            "--rm",
            svgRenderMappings,
            "asciinema/asciicast2gif",
            "-w",
            columns,
            "-h",
            rows,
            "-t",
            "solarized-dark",
            s"/out/$name.cast",
            s"/data/dark/gifs/$name.gif"
          )
            .call(stdin = os.Inherit, stdout = os.Inherit)
        }
      }
    }
  }

}
