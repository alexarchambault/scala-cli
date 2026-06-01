package scala.cli.integration

import com.eed3si9n.expecty.Expecty.expect

class WorkspaceDirNameTests extends ScalaCliSuite {
  override def group: ScalaCliSuite.TestGroup = ScalaCliSuite.TestGroup.First

  private val inputs = TestInputs(
    os.rel / "Hello.scala" ->
      """object Hello {
        |  def main(args: Array[String]): Unit =
        |    println("Hello")
        |}
        |""".stripMargin
  )

  test("SCALA_CLI_WORKSPACE_DIR_NAME overrides the default workspace dir name") {
    inputs.fromRoot { root =>
      val customDirName = ".custom-scala-build"
      val res = os.proc(TestUtil.cli, "run", ".")
        .call(cwd = root, env = Map("SCALA_CLI_WORKSPACE_DIR_NAME" -> customDirName))
      expect(res.out.trim() == "Hello")
      expect(os.exists(root / customDirName))
      expect(!os.exists(root / Constants.workspaceDirName))
    }
  }

  test("SCALA_CLI_WORKSPACE_DIR_NAME accepts a nested sub-path") {
    inputs.fromRoot { root =>
      val customDir = os.sub / "nested" / "build-dir"
      val res = os.proc(TestUtil.cli, "run", ".")
        .call(cwd = root, env = Map("SCALA_CLI_WORKSPACE_DIR_NAME" -> customDir.toString))
      expect(res.out.trim() == "Hello")
      expect(os.exists(root / customDir))
      expect(!os.exists(root / Constants.workspaceDirName))
    }
  }
}
