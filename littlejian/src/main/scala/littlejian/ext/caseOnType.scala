package littlejian.ext

import littlejian._

import scala.reflect.ClassTag

implicit class CaseOnTypeOps[T](x: VarOr[T]) {
  def caseOnType[U <: T, R](isType: VarOr[U] => Rel[R])(isNotType: Rel[R])(implicit t: ClassTag[U]) = ???
}
