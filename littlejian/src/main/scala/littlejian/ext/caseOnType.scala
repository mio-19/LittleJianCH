package littlejian.ext

import littlejian._

import scala.reflect.ClassTag

implicit class CaseOnTypeOps[T, R](x: VarOr[T]) {
  def caseOnType[U <: T](isType: VarOr[U] => Rel[R])(isNotType: Rel[R])(implicit t: ClassTag[U], u: Unify[U], r: Unify[R]) = conde(
    for {
      v <- x.cast[U]
      r <- isType(v)
    } yield r,
    x.isNotType[U] >> isNotType
  )

  def caseOnType[U <: T, A](maker: VarOr[A] => U)(isType: VarOr[A] => Rel[R])(isNotType: Rel[R])
                           (implicit t: ClassTag[U], u: Unify[U], r: Unify[R], uu: Unify[T]) = conde(
    for {
      a <- x.is[A](maker)
      r <- isType(a)
    } yield r,
    x.isNotType[U] >> isNotType
  )

  def caseOnType[U <: T, A, B](maker: (VarOr[A], VarOr[B]) => U)(isType: (VarOr[A], VarOr[B]) => Rel[R])(isNotType: Rel[R])
                              (implicit t: ClassTag[U], u: Unify[U], r: Unify[R], uu: Unify[T]) = conde(
    for {
      (a, b) <- x.is[A, B](maker)
      r <- isType(a, b)
    } yield r,
    x.isNotType[U] >> isNotType
  )
}
