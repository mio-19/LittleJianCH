package littlejian.data

import littlejian._
import scala.language.implicitConversions

sealed trait LList[T] {
  def ::(elem: VarOr[T]): LList[T] = LCons(elem, this)
}

object LList {
  def empty[T]: LList[T] = LEmpty()

  def apply[T](xs: VarOr[T]*): LList[T] = LList.from(xs)

  def from[T](xs: Seq[VarOr[T]]): LList[T] = if (xs.isEmpty) LEmpty() else LCons(xs.head, LList.from(xs.tail))
}

final case class LEmpty[T]() extends LList[T]

final case class LCons[T](head: VarOr[T], tail: VarOr[LList[T]]) extends LList[T]

implicit def U$LList[T](implicit unifier: Unifier[T]): Unifier[LList[T]] = {
  implicit object U extends Unifier[LList[T]] {
    override def concreteUnify(self: LList[T], other: LList[T]): Unifying[Unit] = (self, other) match {
      case (LEmpty(), LEmpty()) => Unifying.success(())
      case (LCons(x, xs), LCons(y, ys)) => for {
        _ <- x.unify(y)
        _ <- xs.unify(ys)
      } yield ()
      case _ => Unifying.failure
    }
  }
  U
}
