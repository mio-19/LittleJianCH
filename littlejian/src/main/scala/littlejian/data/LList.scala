package littlejian.data

import littlejian._
import littlejian.ext._
import scala.language.implicitConversions

sealed trait LList[T] derives Unify, Inspect

implicit class LListOps[T](self: VarOr[LList[T]]) {
  inline def ::(elem: VarOr[T]): LList[T] = LCons(elem, self)

  inline def +:(elem: VarOr[T]): LList[T] = LCons(elem, self)

  def isEmpty(implicit unifier: Unify[LList[T]]): Goal = self === LList.empty

  def elim[U](ifEmpty: Rel[U])(ifNonEmpty: (VarOr[T], VarOr[LList[T]]) => Rel[U])(implicit t: Unify[LList[T]], u: Unify[U]): Rel[U] =
    conde(
      (self === LList.empty) >> ifEmpty,
      for {
        (x, xs) <- self.is[T, LList[T]](LCons(_, _))
        result <- ifNonEmpty(x, xs)
      } yield result
    )

  def head(implicit unifier: Unify[LList[T]]): Rel[T] = for {
    (x, ignored) <- self.is[T, LList[T]](LCons(_, _))
  } yield x

  def getStrings: (String, Vector[String]) | String = {
    val result = self.toString
    if (result.startsWith("LList(") && result.endsWith(")")) {
      (result, result.drop(6).dropRight(1).split(", ").toVector)
    } else {
      result
    }
  }
}

implicit class VarOrLListOps[T](self: VarOr[LList[T]])(implicit U$T: Unify[T]) {
  implicit val U$SeqT: Unify[Seq[VarOr[T]]] = U$Seq(U$VarOr(U$T))

  def eqEmpty: Goal = self === LList.empty[T]

  def append(other: VarOr[LList[T]]): Rel[LList[T]] = self.elim(other) { (x, xs) =>
    for {
      result <- xs.append(other)
    } yield x :: result
  }
}

object LList {
  def empty[T]: LList[T] = LEmpty()

  def apply[T](xs: VarOr[T]*): LList[T] = LList.from(xs)

  def from[T](xs: Seq[VarOr[T]]): LList[T] = if (xs.isEmpty) LEmpty() else LCons(xs.head, LList.from(xs.tail))
}

case class LEmpty[T]() extends LList[T] derives Unify, Inspect {
  override def toString: String = "LList()"
}

case class LCons[T](head: VarOr[T], tail: VarOr[LList[T]]) extends LList[T] derives Unify, Inspect {
  override def toString: String = {
    val t = tail.toString
    if (t == "LList()")
      s"LList(${head})"
    else if (t.startsWith("LList("))
      s"LList(${head}, ${t.drop(6)}"
    else s"LCons(${head}, ${t})"
  }
}

trait LListOf[A] {
  sealed trait LListT

  implicit def LListT2LList(x: LListT): LList[A] = x.asInstanceOf

  val empty: LListT = new EmptyList

  def cons(head: VarOr[A], tail: VarOr[LList[A]]): LListT = new NonemptyList(head, tail)

  final class EmptyList extends LEmpty[A] with LListT

  final class NonemptyList(head: VarOr[A], tail: VarOr[LList[A]]) extends LCons[A](head, tail) with LListT
}
