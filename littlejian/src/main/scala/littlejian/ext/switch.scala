package littlejian.ext

import littlejian._

implicit class SwitchOps[T](x: VarOr[T]) {
  def switch[U](clauses: (VarOr[T], Rel[U])*)(implicit t: Unify[T], u: Unify[U]) = conde(clauses.map { case (on, body) => (x === on) >> body } *)
}