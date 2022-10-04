package littlejian

import littlejian.utils._

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

type Subst = Map[Var[_], (Unifier[_] /* for =/= usages */ , _ /*VarOr[_]*/ )]

type SubstPatch = Vector[(Var[_], Unifier[_], _)]

object SubstPatch {
  val empty: SubstPatch = Vector.empty
}

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

  def addEntryUnchecked(v: Var[_], x: Any)(unifier: Unifier[_]): Subst =
    if (self.contains(v)) throw new IllegalArgumentException("duplicate add") else self.updated(v, (unifier, x))
}

object Subst {
  private var emptyImpl: Subst = HashMap.empty

  def empty: Subst = emptyImpl

  def choseHashMapImpl(): Unit = {
    emptyImpl = HashMap.empty
  }

  def choseImmutableWeakHashMapImpl(): Unit = {
    emptyImpl = ImmutableWeakHashMap.empty
  }

  def walk[T](x: VarOr[T]): Unifying[VarOr[T]] = StateOption {
    case state@(subst, _) => Some((state, subst.walk(x)))
  }

  def addEntry[T](v: Var[T], x: VarOr[T])(implicit unifier: Unifier[T]): Unifying[Unit] = StateOption {
    case (subst, patch) => Some(((subst.addEntry(v, x), (v, unifier, x) +: patch), ()))
  }

  def patch(subst: Subst, p: SubstPatch): Subst = p.foldLeft(subst) {
    case (subst, (v, unifier, x)) => subst.addEntryUnchecked(v, x)(unifier)
  }
}