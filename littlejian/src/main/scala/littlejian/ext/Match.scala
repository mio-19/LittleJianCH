package littlejian.ext

import littlejian._

type Matcher1[R, A] = VarOr[A] => VarOr[R]
type Matcher2[R, A, B] = (VarOr[A], VarOr[B]) => VarOr[R]
type Matcher3[R, A, B, C] = (VarOr[A], VarOr[B], VarOr[C]) => VarOr[R]
type Matcher4[R, A, B, C, D] = (VarOr[A], VarOr[B], VarOr[C], VarOr[D]) => VarOr[R]

implicit class MatchOps[T](self: VarOr[T])(implicit unifier: Unify[T]) {
  @inline def is[A](matcher: Matcher1[T, A]): GoalWith[VarOr[A]] = for {
    a <- fresh[A]
    _ <- self === matcher(a)
  } yield a

  @inline def is[A, B](matcher: Matcher2[T, A, B]): GoalWith[(VarOr[A], VarOr[B])] = for {
    a <- fresh[A]
    b <- fresh[B]
    _ <- self === matcher(a, b)
  } yield (a, b)

  @inline def is[A, B, C](matcher: Matcher3[T, A, B, C]): GoalWith[(VarOr[A], VarOr[B], VarOr[C])] = for {
    a <- fresh[A]
    b <- fresh[B]
    c <- fresh[C]
    _ <- self === matcher(a, b, c)
  } yield (a, b, c)

  @inline def is[A, B, C, D](matcher: Matcher4[T, A, B, C, D]): GoalWith[(VarOr[A], VarOr[B], VarOr[C], VarOr[D])] = for {
    a <- fresh[A]
    b <- fresh[B]
    c <- fresh[C]
    d <- fresh[D]
    _ <- self === matcher(a, b, c, d)
  } yield (a, b, c, d)
}