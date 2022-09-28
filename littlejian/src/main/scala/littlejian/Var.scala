package littlejian

// using reference equality
final class Var[T] {
  override def toString: String = PrettyPrintContext.get match {
    case None => super.toString
    case Some(context) => context.subst.getOption(this) match {
      case Some(x) => x.toString
      case None => "$" + context.getVar(this).toString
    }
  }

  // public api
  def get(state: State): VarOr[T] = state.eq.subst.walk(this)
}

type VarOr[T] = Var[T] | T