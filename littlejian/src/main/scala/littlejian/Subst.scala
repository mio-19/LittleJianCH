package littlejian

import scala.collection.parallel.immutable.ParHashMap

type Subst = ParHashMap[Var[_], UnifiableBox[_]]

implicit class SubstOps(self: Subst) {
  def walk[T](x: VarOr[T])(implicit unifier: Unifier[T]): UnifiableBox[T] = x match {
    case v: Var[_] => self.get(v) match {
      case Some(box) => this.walk(box.x)(box.unifier).asInstanceOf[UnifiableBox[T]]
      case None => UnifiableBox(x, unifier)
    }
    case box: UnifiableBox[_] => this.walk(box.x)(box.unifier).asInstanceOf[UnifiableBox[T]]
    case _ => UnifiableBox(x, unifier)
  }

  def addEntry[T](v: Var[T], x: UnifiableBox[T]): Subst = if (self.contains(v)) throw new IllegalArgumentException("duplicate add") else self.updated(v, x)
}

object Subst {
  val empty: Subst = ParHashMap.empty

  def walk[T](x: VarOr[T])(implicit unifier: Unifier[T]): Unifying[UnifiableBox[T]] = subst => Some((subst, subst.walk(x)(unifier)))

  def addEntry[T](v: Var[T], x: UnifiableBox[T]): Unifying[Unit] = subst => Some((subst.addEntry(v, x), ()))
}