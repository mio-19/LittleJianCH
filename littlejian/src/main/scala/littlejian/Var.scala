package littlejian

// using reference equality
final class Var[T](unifier: Unifier[T]) {
}

type VarOr[T] = Var[T] | T