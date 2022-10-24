package littlejian.unifier

import littlejian._
import scala.language.implicitConversions

implicit def U$Option[T](implicit unifier: Unify[T]): Unify[Option[T]] = (self: Option[T], other: Option[T]) => (self, other) match {
  case (Some(x), Some(y)) => x.unify(y)
  case (None, None) => Unifying.success(())
  case _ => Unifying.failure
}