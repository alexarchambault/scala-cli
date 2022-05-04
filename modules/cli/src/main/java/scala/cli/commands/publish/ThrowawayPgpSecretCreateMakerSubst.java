package scala.cli.internal;

import com.oracle.svm.core.annotate.Substitute;
import com.oracle.svm.core.annotate.TargetClass;
import scala.cli.commands.publish.ThrowawayPgpSecretCreate;

@TargetClass(className = "scala.cli.commands.publish.ThrowawayPgpSecretCreateMaker")
final class ThrowawayPgpSecretCreateMakerSubst {
  @Substitute
  ThrowawayPgpSecretCreate get() {
    return new ThrowawayPgpSecretCreate();
  }
}
