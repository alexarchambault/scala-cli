package scala.cli.config

object Entries {

  val userName  = new Entry.StringEntry(Seq("user"), "name")
  val userEmail = new Entry.StringEntry(Seq("user"), "email")
  val userUrl   = new Entry.StringEntry(Seq("user"), "url")

  val ghToken = new Entry.PasswordEntry(Seq("github"), "token")

  val pgpSecretKey         = new Entry.PasswordEntry(Seq("pgp"), "secret-key")
  val pgpSecretKeyPassword = new Entry.PasswordEntry(Seq("pgp"), "secret-key-password")
  val pgpPublicKey         = new Entry.PasswordEntry(Seq("pgp"), "public-key")

  val sonatypeUser     = new Entry.PasswordEntry(Seq("sonatype"), "user")
  val sonatypePassword = new Entry.PasswordEntry(Seq("sonatype"), "password")

  def all = Seq[Entry[_]](
    userName,
    userEmail,
    userUrl,
    ghToken,
    pgpSecretKey,
    pgpSecretKeyPassword,
    pgpPublicKey,
    sonatypeUser,
    sonatypePassword
  )

  lazy val map = all.map(e => e.fullName -> e).toMap

}
