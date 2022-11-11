package littlejian

import littlejian.utils.Parameter

import scala.collection.immutable.HashSet
import scala.reflect.ClassTag

trait DeepWalk[T] {
  def walk(self: T): T
}

// DeepWalk instances must follow this property
implicit def DeepWalkCovariance[T, U <: T](implicit x: DeepWalk[T]): DeepWalk[U] = x.asInstanceOf

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

trait AtomDeepWalk[T] extends DeepWalk[T] {
  final override def walk(self: T): T = self
}

implicit object W$Unit extends AtomDeepWalk[Unit]

implicit object W$Boolean extends AtomDeepWalk[Boolean]

implicit object W$Short extends AtomDeepWalk[Short]

implicit object W$String extends AtomDeepWalk[String]

def W$Union[T, U](tr: => DeepWalk[T], ur: => DeepWalk[U])(implicit tev: ClassTag[T], uev: ClassTag[U]): DeepWalk[T | U] = {
  lazy val t = tr
  lazy val u = ur
  val tc = tev.runtimeClass
  val uc = uev.runtimeClass
  if (Set(tc, uc).size != 2) throw new IllegalArgumentException("duplication")
  (self) => {
    if (tc.isInstance(self)) t.walk(self.asInstanceOf)
    else u.walk(self.asInstanceOf)
  }
}

def W$Union[A, B, C](ar: => DeepWalk[A], br: => DeepWalk[B], cr: => DeepWalk[C])(implicit aev: ClassTag[A], bev: ClassTag[B], cev: ClassTag[C]): DeepWalk[A | B | C] = {
  lazy val a = ar
  lazy val b = br
  lazy val c = cr
  val ac = aev.runtimeClass
  val bc = bev.runtimeClass
  val cc = cev.runtimeClass
  if (Set(ac, bc, cc).size != 3) throw new IllegalArgumentException("duplication")
  (self) => {
    if (ac.isInstance(self)) a.walk(self.asInstanceOf)
    else if (bc.isInstance(self)) b.walk(self.asInstanceOf)
    else c.walk(self.asInstanceOf)
  }
}
def W$Union[A, B, C, D](ar: => DeepWalk[A], br: => DeepWalk[B], cr: => DeepWalk[C], dr: => DeepWalk[D])(implicit aev: ClassTag[A], bev: ClassTag[B], cev: ClassTag[C], dev: ClassTag[D]): DeepWalk[A | B | C | D] = {
  lazy val a = ar
  lazy val b = br
  lazy val c = cr
  lazy val d = dr
  val ac = aev.runtimeClass
  val bc = bev.runtimeClass
  val cc = cev.runtimeClass
  val dc = dev.runtimeClass
  if (Set(ac, bc, cc, dc).size != 4) throw new IllegalArgumentException("duplication")
  (self) => {
    if (ac.isInstance(self)) a.walk(self.asInstanceOf)
    else if (bc.isInstance(self)) b.walk(self.asInstanceOf)
    else if (cc.isInstance(self)) c.walk(self.asInstanceOf)
    else d.walk(self.asInstanceOf)
  }
}
