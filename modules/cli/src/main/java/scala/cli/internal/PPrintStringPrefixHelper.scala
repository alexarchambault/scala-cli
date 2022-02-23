package scala.cli.internal

final class PPrintStringPrefixHelper {
  def apply(i: Iterable[_]): String =
    i.collectionClassName
}
