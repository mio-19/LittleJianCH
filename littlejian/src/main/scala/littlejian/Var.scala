package littlejian

private val EMPTY_SYMBOL = Symbol("")

final class Var[T](identifier: Symbol, unifier: Unifier[T]) {
  def this(unifier: Unifier[T]) = {
    this(EMPTY_SYMBOL, unifier)
  }
}

type VarOr[T] = Var[T] | T