package scala.cli.integration

import com.eed3si9n.expecty.Expecty.expect

import java.nio.charset.Charset

import scala.util.Properties

abstract class ExportMillTestDefinitions(val scalaVersionOpt: Option[String])
    extends ScalaCliSuite with TestScalaVersionArgs {

  protected lazy val extraOptions: Seq[String] = scalaVersionArgs ++ TestUtil.extraOptions

  protected def runExportTests: Boolean =
    Properties.isLinux

  protected def launcher: os.RelPath =
    if (Properties.isWin) os.rel / "mill.bat"
    else os.rel / "mill"

  private def addMillJvmOpts(inputs: TestInputs): TestInputs =
    inputs.add(
      os.rel / ".mill-jvm-opts" ->
        """-Xmx512m
          |-Xms128m
          |""".stripMargin
    )

  protected def simpleTest(
    inputs: TestInputs,
    extraExportArgs: Seq[String] = Nil,
    millArgs: Seq[String] = Seq("project.run")
  ): Unit =
    addMillJvmOpts(inputs).fromRoot { root =>
      os.proc(
        TestUtil.cli,
        "export",
        extraOptions,
        "--mill",
        "-o",
        "mill-proj",
        ".",
        extraExportArgs
      )
        .call(cwd = root, stdout = os.Inherit)
      val res = os.proc(root / "mill-proj" / launcher, "-i", millArgs)
        .call(cwd = root / "mill-proj")
      val output = res.out.text(Charset.defaultCharset())
      expect(output.contains("Hello from exported Scala CLI project"))
    }

  def jvmTest(projectName: String = "project"): Unit = {
    val inputs = addMillJvmOpts(ExportTestProjects.jvmTest(actualScalaVersion))
    inputs.fromRoot { root =>
      val setProject = if (projectName != "project") Seq("-p", projectName) else Seq.empty
      os.proc(TestUtil.cli, "export", extraOptions, "--mill", setProject, "-o", "mill-proj", ".")
        .call(cwd = root, stdout = os.Inherit)
      locally {
        // main
        val res =
          os.proc(root / "mill-proj" / launcher, s"$projectName.run").call(cwd = root / "mill-proj")
        val output = res.out.text(Charset.defaultCharset())
        expect(output.contains("Hello from " + actualScalaVersion))
        // resource
        expect(output.contains("resource:1,2"))
      }
      locally {
        // scalacOptions
        val res =
          os.proc(
            root / "mill-proj" / launcher,
            "--disable-ticker",
            "show",
            s"$projectName.scalacOptions"
          ).call(cwd = root / "mill-proj")
        val output = res.out.text(Charset.defaultCharset())
        expect(output.filterNot(_.isWhitespace) == "[\"-deprecation\"]")
      }
      locally {
        // test
        val res =
          os.proc(root / "mill-proj" / launcher, s"$projectName.test").call(cwd =
            root / "mill-proj"
          )
        val output = res.out.text(Charset.defaultCharset())
        expect(output.contains("1 succeeded"))
      }
    }
  }
  if (runExportTests)
    test("JVM") {
      jvmTest()
    }

  if (runExportTests)
    test("JVM custom project name") {
      jvmTest("newproject")
    }

  if (runExportTests)
    test("Scala.js") {
      simpleTest(ExportTestProjects.jsTest(actualScalaVersion))
    }

  if (runExportTests && !actualScalaVersion.startsWith("3."))
    test("Scala Native") {
      simpleTest(ExportTestProjects.nativeTest(actualScalaVersion))
    }

}
