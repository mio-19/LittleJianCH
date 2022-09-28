package littlejian

// using reference equality
final class Var[T](implicit unifier: Unifier[T]) {
  override def toString: String = PrettyPrintContext.get match {
    case None => super.toString
    case Some(context) => context.subst.getOption(this) match {
      case Some(box) => box.x.toString
      case None => "$" + context.getVar(this).toString
    }
  }
}

def hole[T](implicit unifier: Unifier[T]) = new Var[T]()(unifier)

type VarOr[T] = Var[T] | T