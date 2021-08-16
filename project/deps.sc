import mill._, scalalib._

object Scala {
  def scala212 = "2.12.14"
  def scala213 = "2.13.6"
  def scala3 = "3.0.1"
  val allScala2 = Seq(scala213, scala212)
  val all = allScala2 ++ Seq(scala3)

  def listAll: Seq[String] =
    (6 to 13).map(i => s"2.12.$i") ++ Seq(scala212) ++
      (0 to 5).map(i => s"2.13.$i") ++ Seq(scala213) ++
      (0 to 0).map(i => s"3.0.$i") ++ Seq(scala3)

  // The Scala version used to build the CLI itself.
  // We should be able to switch to 2.13.x when bumping the scala-native version.
  def defaultInternal = scala212

  // The Scala version used by default to compile user input.
  def defaultUser = scala3
}

object Deps {
  object Versions {
    def coursier = "2.0.16+73-gddc6d9cc9"
    def scalaJs = "1.5.1"
    def scalaNative = "0.4.0"
    def scalaPackager = "0.1.17"
  }
  def ammonite = ivy"com.lihaoyi:::ammonite:2.4.0-12-69f45b4b"
  def asm = ivy"org.ow2.asm:asm:9.1"
  def bloopConfig = ivy"ch.epfl.scala::bloop-config:1.4.8-83-27d9d739"
  def bsp4j = ivy"ch.epfl.scala:bsp4j:2.0.0-M13"
  def caseApp = ivy"com.github.alexarchambault::case-app:2.1.0-M6"
  def coursierJvm = ivy"io.get-coursier::coursier-jvm:${Versions.coursier}"
  def coursierLauncher = ivy"io.get-coursier::coursier-launcher:${Versions.coursier}"
  def dataClass = ivy"io.github.alexarchambault::data-class:0.2.5"
  def dependency = ivy"io.get-coursier::dependency:0.2.0"
  def expecty = ivy"com.eed3si9n.expecty::expecty:0.15.1"
  def guava = ivy"com.google.guava:guava:18.0"
  def ipcSocket = ivy"org.scala-sbt.ipcsocket:ipcsocket:1.4.0"
  def jimfs = ivy"com.google.jimfs:jimfs:1.2"
  def jniUtils = ivy"io.get-coursier.jniutils:windows-jni-utils:0.2.5"
  def macroParadise = ivy"org.scalamacros:::paradise:2.1.1"
  def munit = ivy"org.scalameta::munit:0.7.25"
  def nativeTestRunner = ivy"org.scala-native::test-runner:${Versions.scalaNative}"
  def nativeTools = ivy"org.scala-native::tools:${Versions.scalaNative}"
  def osLib = ivy"com.lihaoyi::os-lib:0.7.5"
  def pprint = ivy"com.lihaoyi::pprint:0.6.6"
  def prettyStacktraces = ivy"org.virtuslab::pretty-stacktraces:0.0.0+27-b9d69198-SNAPSHOT"
  def pureconfig = ivy"com.github.pureconfig::pureconfig-generic-base:0.16.0"
  def scala3Compiler(sv: String) = ivy"org.scala-lang::scala3-compiler:$sv"
  def scalaAsync = ivy"org.scala-lang.modules::scala-async:0.10.0"
  def scalac(sv: String) = ivy"org.scala-lang:scala-compiler:$sv"
  def scalaJsEnvNodeJs = ivy"org.scala-js::scalajs-env-nodejs:1.1.1"
  def scalaJsLinker = ivy"org.scala-js::scalajs-linker:${Versions.scalaJs}"
  def scalaJsLinkerInterface = ivy"org.scala-js::scalajs-linker-interface:${Versions.scalaJs}"
  def scalaJsTestAdapter = ivy"org.scala-js::scalajs-sbt-test-adapter:${Versions.scalaJs}"
  def scalametaTrees = ivy"org.scalameta::trees:4.4.21"
  def scalaPackager = ivy"org.virtuslab::scala-packager:${Versions.scalaPackager}"
  def scalaPackagerCli = ivy"org.virtuslab::scala-packager-cli:${Versions.scalaPackager}"
  def scalaparse = ivy"com.lihaoyi::scalaparse:2.3.2"
  def shapeless = ivy"com.chuusai::shapeless:2.3.7"
  def slf4jNop = ivy"org.slf4j:slf4j-nop:1.8.0-beta4"
  def snailgun = ivy"me.vican.jorge::snailgun-core:0.4.0"
  def svm = ivy"org.graalvm.nativeimage:svm:$graalVmVersion"
  def svmSubs = ivy"org.scalameta::svm-subs:20.2.0"
  def swoval = ivy"com.swoval:file-tree-views:2.1.5"
  def testInterface = ivy"org.scala-sbt:test-interface:1.0"
  def upickle = ivy"com.lihaoyi::upickle:1.3.8"
  def usingDirectives = ivy"org.virtuslab:using_directives:0.0.2-20210816.074544-1"
}

def graalVmVersion = "21.2.0"

def csDockerVersion  = "2.0.16"

object Docker {
  def muslBuilder   = "messense/rust-musl-cross@sha256:12d0dd535ef7364bf49cb2608ae7eaf60e40d07834eb4d9160c592422a08d3b3"

  def testImage = "ubuntu:18.04"
  def alpineTestImage = "alpine@sha256:234cb88d3020898631af0ccbbcca9a66ae7306ecd30c9720690858c1b007d2a0"
}
