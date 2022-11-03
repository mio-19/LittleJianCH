package littlejian.ext

import littlejian._

trait Force[T] {
  def force(x: VarOr[T]): GoalWith[T]
}

implicit class ForceOps[T](x: VarOr[T])(implicit f: Force[T]) {
  def force: GoalWith[T] = f.force(x)
}

trait Freshable[T] {
  def doFresh: GoalWith[T]
}

implicit def FreshableForVarOr[T]: Freshable[VarOr[T]] = new Freshable[VarOr[T]] {
  def doFresh: GoalWith[VarOr[T]] = for {
    v <- fresh[T]
  } yield v
}

// I don't know how to implement this
object Force {
  import shapeless3.deriving.*

  given forceSum[A] (using inst: K0.CoproductInstances[Freshable, A]): Force[A] with
    def force(x: VarOr[A]): GoalWith[A] = ??? // TODO

  given forceProduct[A] (using inst: K0.ProductInstances[Freshable, A]): Force[A] with
    def force(x: VarOr[A]): GoalWith[A] = ??? // TODO // inst.construct([t] => (f: Freshable[t]) => f.doFresh)

  inline def derived[A](using gen: K0.Generic[A]): Force[A] =
    gen.derive(forceProduct, forceSum)
}