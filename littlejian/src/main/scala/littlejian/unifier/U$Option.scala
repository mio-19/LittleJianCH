package littlejian.unifier


import littlejian._

import scala.language.implicitConversions

implicit def U$Option[T](implicit unifier: Unifier[T]): Unifier[Option[T]] = new Unifier[Option[T]] {
  override def concreteUnify(self: Option[T], other: Option[T]): Unifying[Unit] = (self, other) match {
    case (Some(x), Some(y)) => x.unify(y)
    case (None, None) => Unifying.success(())
    case _ => Unifying.failure
  }
}