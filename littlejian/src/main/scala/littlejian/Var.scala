package littlejian

// using reference equality
final class Var[T] {
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