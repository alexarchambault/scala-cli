package scala.cli.internal;

import com.oracle.svm.core.annotate.Substitute;
import com.oracle.svm.core.annotate.TargetClass;

@TargetClass(className = "pprint.StringPrefix$")
final class PPrintStringPrefixSubst {

  @Substitute
  String apply(scala.collection.Iterable<?> i) {
    String name = (new PPrintStringPrefixHelper()).apply(i);
    return name;
  }

}
