package scala.build.preprocessing

import com.virtuslab.using_directives.custom.model.{Path, StringValue, Value}
import dependency.AnyDependency
import dependency.parser.DependencyParser

import scala.build.options.{BuildOptions, ClassPathOptions, ScalaOptions}
import scala.collection.JavaConverters._

object DirectivesProcessor {

  private val processors = Map(
    "lib" -> (processLib _),
    "scala" -> (processScala _)
  )

  private def processLib(value: Any): BuildOptions = {

    val extraDependencies = Some(value)
      .collect {
        case list: java.util.List[_] =>
          list.asScala.collect { case s: String => s }.toVector
        case s: String =>
          Vector(s)
      }
      .map { deps =>
        val deps0 = deps.map(_.filter(!_.isSpaceChar)) // Really necessary? (might already be handled by the coursier-dependency library)
        deps0.map(parseDependency)
      }
      .getOrElse(Vector.empty)

    BuildOptions(
      classPathOptions = ClassPathOptions(
        extraDependencies = extraDependencies
      )
    )
  }

  private def processScala(value: Any): BuildOptions = {

    val versions = Some(value)
      .toList
      .collect {
        case list: java.util.List[_] =>
          list
            .asScala
            .collect {
              case v: String => v // needed?
              case v: StringValue => v.get()
            }
            .toList
        case v: StringValue => List(v.get())
        case v: String => List(v) // needed?
      }
      .flatten
      .map(_.filter(!_.isSpaceChar))
      .filter(_.nonEmpty)
      .distinct

    versions match {
      case Nil => BuildOptions()
      case v :: Nil =>
        BuildOptions(
          scalaOptions = ScalaOptions(
            scalaVersion = Some(v)
          )
        )
      case _ =>
        val highest = versions.maxBy(coursier.core.Version(_))
        BuildOptions(
          scalaOptions = ScalaOptions(
            scalaVersion = Some(highest),
            extraScalaVersions = versions.iterator.filter(_ != highest).toSet
          )
        )
    }
  }

  def process(directives: Map[Path, Value[_]]): BuildOptions = {

    val values = directives.map {
      case (k, v) =>
        k.getPath.asScala.mkString(".") -> (v.get: Any)
    }

    values
      .iterator
      .flatMap {
        case (k, v) =>
          processors.get(k).iterator.map { f =>
            f(v)
          }
      }
      .foldLeft(BuildOptions())(_ orElse _)
  }

  private def parseDependency(str: String): AnyDependency =
    DependencyParser.parse(str) match {
      case Left(msg) => sys.error(s"Malformed dependency '$str': $msg")
      case Right(dep) => dep
    }
}
