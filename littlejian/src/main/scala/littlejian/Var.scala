package littlejian

// using reference equality
final class Var[T](implicit unifier: Unifier[T]) {
  override def toString: String = PrettyPrintContext.get match {
    case None => super.toString
    case Some(context) => ??? // TODO
  }
}

def hole[T](implicit unifier: Unifier[T]) = new Var[T]()(unifier)

type VarOr[T] = Var[T] | T