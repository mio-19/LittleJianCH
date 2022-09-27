package littlejian

// using reference equality
final class Var[T](implicit unifier: Unifier[T]) {
}

def hole[T](implicit unifier: Unifier[T]) = new Var[T]()(unifier)

type VarOr[T] = Var[T] | T