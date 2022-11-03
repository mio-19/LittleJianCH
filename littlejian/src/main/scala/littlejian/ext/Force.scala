package littlejian.ext

import littlejian._

trait Force[T] {
  def force(x: VarOr[T]): GoalWith[T]
}

implicit class ForceOps[T](x: VarOr[T])(implicit f: Force[T]) {
  def force: GoalWith[T] = f.force(x)
}
