package scala.cli.commands.pgp

import caseapp.core.RemainingArgs
import org.bouncycastle.bcpg.ArmoredOutputStream

import java.io.{BufferedOutputStream, ByteArrayOutputStream, File}

import scala.cli.commands.ScalaCommand

object PgpCreate extends ScalaCommand[PgpCreateOptions] {

  override def inSipScala = false
  override def hidden     = true
  override def names = List(
    List("pgp", "create")
  )

  private def printable(p: os.Path): String =
    if (p.startsWith(os.pwd)) p.relativeTo(os.pwd).segments.mkString(File.separator)
    else p.toString

  def run(options: PgpCreateOptions, args: RemainingArgs): Unit = {

    val pass       = options.password.get().value.toCharArray
    val keyRingGen = PgpHelper.generateKeyRingGenerator(options.email, pass)
    val pubKeyRing = keyRingGen.generatePublicKeyRing()

    val pubKeyContent = {
      val baos = new ByteArrayOutputStream
      val out  = new ArmoredOutputStream(baos)
      pubKeyRing.encode(out)
      out.close()
      baos.toByteArray
    }
    val secretKeyContent = {
      val baos   = new ByteArrayOutputStream
      val skr    = keyRingGen.generateSecretKeyRing()
      val secout = new BufferedOutputStream(baos)
      skr.encode(secout)
      secout.close()
      baos.toByteArray
    }

    val publicKeyPath = options.publicKeyPath
    val secretKeyPath = options.secretKeyPath

    os.write(publicKeyPath, pubKeyContent)
    System.err.println(s"Wrote public key to ${printable(publicKeyPath)}")
    os.write(secretKeyPath, secretKeyContent)
    System.err.println(s"Wrote secret key to ${printable(secretKeyPath)}")
  }
}
