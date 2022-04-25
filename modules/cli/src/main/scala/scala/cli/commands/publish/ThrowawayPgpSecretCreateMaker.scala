package scala.cli.commands.publish

class ThrowawayPgpSecretCreateMaker {
  def get(): ThrowawayPgpSecretCreate =
    new ThrowawayPgpSecretCreateJvm
}