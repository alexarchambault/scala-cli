package scala.cli.commands.packaging

import dependency._

object Spark {

  private def names = Seq(
    // FIXME Add more? (see "cs complete-dependency org.apache.spark: | grep '_2\.12$'")
    "core",
    "graph",
    "graphx",
    "hive",
    "kubernetes",
    "mesos",
    "repl",
    "sql",
    "yarn"
  )

  def sparkModules: Seq[AnyModule] =
    names.map(name => mod"org.apache.spark::spark-$name")
}
