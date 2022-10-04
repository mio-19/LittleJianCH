package littlejian.data

import littlejian._
import littlejian.ext._
import littlejian.unifier._
import scala.language.implicitConversions

sealed trait LList[T] {
  def ::(elem: VarOr[T]): LList[T] = LCons(elem, this)

}

implicit class VarOrLListOps[T](self: VarOr[LList[T]])(implicit U$T: Unifier[T]) {
  implicit val U$LListT: Unifier[LList[T]] = U$LList(U$T)
  implicit val U$SeqT: Unifier[Seq[VarOr[T]]] = U$Seq(U$VarOr(U$T))

  def isEmpty: Rel[Boolean] = conde(
    for {
      _ <- self === LList.empty[T]
    } yield true,
    for {
      _ <- self.is[T, LList[T]](LCons(_, _))
    } yield false
  )

  @deprecated
  def toSeq: Rel[Seq[VarOr[T]]] = conde(
    for {
      _ <- self === LEmpty[T]()
    } yield Seq.empty,
    for {
      (head, tail) <- self.is[T, LList[T]](LCons(_, _))
      tailSeq <- tail.toSeq
      result <- tailSeq.forceApply[Seq[VarOr[T]]](head +: _)
    } yield result
  )
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
