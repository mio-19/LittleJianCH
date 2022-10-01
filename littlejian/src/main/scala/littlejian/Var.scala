package littlejian

// using reference equality
final class Var[T] private[littlejian] {
  override def toString: String =
    ToStringRecCatcher.record(this, (id, x) => s"#$id=$x", id => s"#$id") {
      prettyPrintContext.get match {
        case None => super.toString
        case Some(context) => context.subst.getOption(this) match {
          case Some(x) => x.toString
          case None => "$" + context.getVar(this).toString
        }
      }
    }
}

type VarOr[T] = Var[T] | T


def hole[T]: VarOr[T] = new Var[T]
def callWithFresh[T](f: VarOr[T] => Goal): Goal = f(new Var[T])