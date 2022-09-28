package littlejian.unifier

import littlejian._

import scala.language.implicitConversions

implicit def U$List[T](implicit unifier: Unifier[T]): Unifier[List[T]] = {
  implicit object U extends Unifier[List[T]] {
    override def concreteUnify(self: List[T], other: List[T]): Unifying[Unit] = (self, other) match {
      case (x :: xs, y :: ys) => for {
        _ <- x.unify(y)
        _ <- xs.unify(ys)
      } yield ()
      case (Nil, Nil) => Unifying.success(())
      case _ => Unifying.failure
    }
  }
  U
}