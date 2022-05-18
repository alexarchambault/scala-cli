package scala.cli.config

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._
import coursier.parse.RawJson

import java.nio.file.attribute.PosixFilePermission

import scala.build.Directories
import scala.build.errors.BuildException
import scala.collection.immutable.ListMap

final class ConfigDb private (
  var rawEntries: Map[String, Array[Byte]]
) {

  def get[T](entry: Entry[T]): Either[ConfigDb.ConfigDbFormatError, Option[T]] =
    rawEntries.get(entry.fullName) match {
      case None => Right(None)
      case Some(rawEntryContent) =>
        entry.parse(rawEntryContent)
          .left.map { e =>
            new ConfigDb.ConfigDbFormatError(s"Error parsing ${entry.fullName} value", Some(e))
          }
          .map(Some(_))
    }

  def set[T](entry: Entry[T], value: T): this.type = {
    val b = entry.write(value)
    rawEntries += entry.fullName -> b
    this
  }

  def getAsString[T](entry: Entry[T]): Either[ConfigDb.ConfigDbFormatError, Option[Seq[String]]] =
    get(entry).map(_.map(entry.asString))

  def setFromString[T](
    entry: Entry[T],
    values: Seq[String]
  ): Either[Entry.MalformedEntry, this.type] =
    entry.fromString(values).map { typedValue =>
      set(entry, typedValue)
    }

  def dump: Array[Byte] = {

    def serializeMap(m: Map[String, Array[Byte]]): Array[Byte] = {
      val keyValues = m
        .groupBy(_._1.split("\\.", 2).apply(0))
        .toVector
        .sortBy(_._1)
        .map {
          case (k, v) =>
            val v0 = v.map {
              case (k1, v1) =>
                (k1.stripPrefix(k).stripPrefix("."), v1)
            }
            (k, serialize(v0))
        }
      val sortedMap: Map[String, RawJson] = ListMap.from(keyValues)
      writeToArray(sortedMap)(ConfigDb.codec)
    }

    def serialize(m: Map[String, Array[Byte]]): RawJson =
      m.get("") match {
        case Some(value) =>
          if (m.size == 1)
            RawJson(value)
          else
            sys.error(s"Inconsistent keys: ${m.keySet.toVector.sorted}")
        case None =>
          RawJson(serializeMap(m))
      }

    serializeMap(rawEntries)
  }

  def saveUnsafe(path: os.Path): Either[ConfigDb.ConfigDbPermissionsError, Unit] = {
    val dir = path / os.up
    if (!os.exists(dir))
      os.makeDir(dir, "rwx------")
    val dirPerms = os.perms(dir)
    val hasWrongPerms =
      dirPerms.contains(PosixFilePermission.GROUP_READ) ||
      dirPerms.contains(PosixFilePermission.GROUP_WRITE) ||
      dirPerms.contains(PosixFilePermission.GROUP_EXECUTE) ||
      dirPerms.contains(PosixFilePermission.OTHERS_READ) ||
      dirPerms.contains(PosixFilePermission.OTHERS_WRITE) ||
      dirPerms.contains(PosixFilePermission.OTHERS_EXECUTE)
    if (hasWrongPerms)
      Left(new ConfigDb.ConfigDbPermissionsError(path, dirPerms))
    else {
      os.write.over(path, dump, perms = "rw-------", createFolders = false)
      Right(())
    }
  }
  def save(directories: Directories): Either[BuildException, Unit] = {
    // file locks…
    val path = ConfigDb.dbPath(directories)
    saveUnsafe(path)
  }
}

object ConfigDb {

  def dbPath(directories: Directories): os.Path =
    directories.secretsDir / defaultDbFileName

  final class ConfigDbFormatError(
    message: String,
    causeOpt: Option[Throwable] = None
  ) extends BuildException(message, cause = causeOpt.orNull)

  final class ConfigDbPermissionsError(path: os.Path, perms: os.PermSet)
      extends BuildException(s"$path has wrong permissions $perms (expected rwx------)")

  private val codec: JsonValueCodec[Map[String, RawJson]] = JsonCodecMaker.make

  def apply(
    dbContent: Array[Byte],
    printablePath: Option[String] = None
  ): Either[ConfigDbFormatError, ConfigDb] = {

    def flatten(map: Map[String, RawJson]): Map[String, Array[Byte]] =
      map.flatMap {
        case (k, v) =>
          try {
            val subMap = flatten(readFromArray(v.value)(codec))
            subMap.toSeq.map {
              case (k0, v0) =>
                (k + "." + k0, v0)
            }
          }
          catch {
            case _: JsonReaderException =>
              Seq(k -> v.value)
          }
      }

    val maybeRawEntries =
      try Right(flatten(readFromArray(dbContent)(codec)))
      catch {
        case e: JsonReaderException =>
          Left(new ConfigDbFormatError(
            "Error parsing config DB" + printablePath.fold("")(" " + _),
            Some(e)
          ))
      }

    maybeRawEntries.map(rawEntries => new ConfigDb(rawEntries))
  }

  def defaultDbFileName: String =
    "config.json"

  def open(path: os.Path): Either[BuildException, ConfigDb] =
    if (os.exists(path))
      apply(os.read.bytes(path), Some(path.toString))
    else
      Right(new ConfigDb(Map()))
  def open(directories: Directories): Either[BuildException, ConfigDb] =
    open(dbPath(directories))
}
