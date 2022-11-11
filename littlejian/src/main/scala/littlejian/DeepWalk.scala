package littlejian

import littlejian.utils.Parameter

import scala.collection.immutable.HashSet

trait DeepWalk[T] {
  def walk(self: T): T
}

object DeepWalk {

  import shapeless3.deriving.*

  given walkSum[A] (using inst: K0.CoproductInstances[DeepWalk, A]): DeepWalk[A] with
    def walk(self: A): A = inst.fold(self)(
      [t] => (w: DeepWalk[t], t0: t) => w.walk(t0).asInstanceOf
    )

  given walkProduct[A] (using inst: K0.ProductInstances[DeepWalk, A]): DeepWalk[A] with
    def walk(self: A): A = inst.map(self)(
      [t] => (w: DeepWalk[t], t0: t) => w.walk(t0)
    )

  inline def derived[A](using gen: K0.Generic[A]): DeepWalk[A] =
    gen.derive(walkProduct, walkSum)
}

private val W$VarRec = new Parameter[HashSet[Var[_]]]
private val walkSubst = new Parameter[Subst]

implicit def W$VarOr[T](implicit walker: DeepWalk[T]): DeepWalk[VarOr[T]] = new DeepWalk[VarOr[T]]() {
  override def walk(self0: VarOr[T]): VarOr[T] = {
    if (!self0.isInstanceOf[Var[_]]) return walker.walk(self0.asInstanceOf)
    val self: Var[T] = self0.asInstanceOf
    val history = W$VarRec.get.getOrElse(HashSet.empty)
    if (history.contains(self)) return self
    val subst0 = walkSubst.get
    if (subst0.isEmpty) return self
    val subst = subst0.get
    val newSelf = subst.walk(self)
    newSelf match {
      case x: Var[_] => x.asInstanceOf
      case x => W$VarRec.callWith(history.incl(self)) {
        walker.walk(x.asInstanceOf)
      }
    }
  }
}

implicit class DeepWalkInfix[T](x: T)(implicit walker: DeepWalk[T]) {
  def deepWalk(subst: Subst): T = walkSubst.callWith(subst) {
    walker.walk(x)
  }
}
