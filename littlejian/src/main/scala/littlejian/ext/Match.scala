package littlejian.ext

import littlejian._

type Matcher1[R, A] = VarOr[A] => R
type Matcher2[R, A, B] = (VarOr[A], VarOr[B]) => R
type Matcher3[R, A, B, C] = (VarOr[A], VarOr[B], VarOr[C]) => R

implicit class MatchOps[T](self: VarOr[T])(implicit unifier: Unifier[T]) {
  def is[A](matcher: Matcher1[T, A]): GoalWith[VarOr[A]] = {
    val a = hole[A]
    begin(self === matcher(a), a)
  }

  def is[A, B](matcher: Matcher2[T, A, B]): GoalWith[(VarOr[A], VarOr[B])] = {
    val a = hole[A]
    val b = hole[B]
    begin(self === matcher(a, b), (a, b))
  }

  def is[A, B, C](matcher: Matcher3[T, A, B, C]): GoalWith[(VarOr[A], VarOr[B], VarOr[C])] = {
    val a = hole[A]
    val b = hole[B]
    val c = hole[C]
    begin(self === matcher(a, b, c), (a, b, c))
  }
}