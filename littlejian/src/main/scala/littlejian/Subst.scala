package littlejian

import scala.annotation.tailrec
import scala.collection.parallel.immutable.ParHashMap

type Subst = ParHashMap[Var[_], (Unifier[_]/* for =/= usages */, _/*VarOr[_]*/)]

implicit final class SubstOps(self: Subst) {
  @tailrec
  def walk[T](x: VarOr[T]): VarOr[T] = x match {
    case v: Var[_] => self.get(v) match {
      case Some((_, v)) => walk(v.asInstanceOf[VarOr[T]])
      case None => x
    }
    case _ => x
  }

  def getOption[T](x: Var[T]): Option[VarOr[T]] = self.get(x) match {
    case Some((_, v)) => Some(walk(v.asInstanceOf[VarOr[T]]))
    case None => None
  }

  def addEntry[T](v: Var[T], x: VarOr[T])(implicit unifier: Unifier[T]): Subst =
    if (self.contains(v)) throw new IllegalArgumentException("duplicate add") else self.updated(v, (unifier, x))
}

object Subst {
  val empty: Subst = ParHashMap.empty

  def walk[T](x: VarOr[T]): Unifying[VarOr[T]] = subst => Some((subst, subst.walk(x)))

  def addEntry[T](v: Var[T], x: VarOr[T])(implicit unifier: Unifier[T]): Unifying[Unit] = subst => Some((subst.addEntry(v, x), ()))
}