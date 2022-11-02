package littlejian

import littlejian.utils.*

import scala.annotation.{tailrec, targetName}
import scala.language.implicitConversions

// Monad
type Unifying[T] = StateOption[(Subst, SubstPatch), T]

object Unifying {
  inline def success[T](x: T): Unifying[T] = StateOption.success(x)

  inline def failure[T]: Unifying[T] = StateOption.failure

  inline def guard(x: Boolean): Unifying[Unit] = StateOption.guard(x)

  inline def runAll(xs: Seq[Unifying[Unit]]): Unifying[Unit] = xs.fold(success(()))(_ >> _)
}

implicit class UnifyingOps[T](self: Unifying[T]) {
  def run(subst: Subst): Option[((Subst, SubstPatch), T)] = self.run((subst, SubstPatch.empty))

  def getSubst(subst: Subst): Option[Subst] = self.run((subst, SubstPatch.empty)).map(_._1._1)

  def getSubstWithPatch(subst: Subst): Option[(Subst, SubstPatch)] = self.run((subst, SubstPatch.empty)).map(_._1)

  def getSubstPatch(subst: Subst): Option[SubstPatch] = self.run((subst, SubstPatch.empty)).map(_._1._2)

  inline def map[U](f: T => U): Unifying[U] = self.map(f)

  inline def flatMap[U](f: T => Unifying[U]): Unifying[U] = self.flatMap(f)

  inline def >>[U](that: Unifying[U]): Unifying[U] = self >> that
}

trait Unify[T] {
  final implicit val thisUnifier: Unify[T] = this

  final def unify(self: VarOr[T], other: VarOr[T]): Unifying[Unit] = for {
    self <- Subst.walk(self)
    other <- Subst.walk(other)
    _ <- (self, other) match {
      case _ if self == other => Unifying.success(())
      case (self: Var[_], _) => Subst.addEntry(self.asInstanceOf[Var[T]], other)
      case (_, other: Var[_]) => Subst.addEntry(other.asInstanceOf[Var[T]], self)
      case _ => concreteUnify(self.asInstanceOf[T], other.asInstanceOf[T])
    }
  } yield ()

  def concreteUnify(self: T, other: T): Unifying[Unit]
}

implicit class InfixUnify[T](self: VarOr[T])(implicit unifier: Unify[T]) {
  def unify(other: VarOr[T]): Unifying[Unit] = unifier.unify(self, other)
}

object Unify {
  import shapeless3.deriving.*
  given unifySum[A] (using inst: K0.CoproductInstances[Unify, A]): Unify[A] with
    def concreteUnify(x: A, y: A): Unifying[Unit] = inst.fold2(x, y)(Unifying.failure: Unifying[Unit])(
      [t] => (u: Unify[t], t0: t, t1: t) => u.unify(t0, t1)
    )

  given unifyProduct[A] (using inst: K0.ProductInstances[Unify, A]): Unify[A] with
    def concreteUnify(x: A, y: A): Unifying[Unit] = inst.foldLeft2(x, y)(Unifying.success(()))(
      [t] => (acc: Unifying[Unit], u: Unify[t], t0: t, t1: t) =>
        acc >> u.unify(t0, t1)
    )

  inline def derived[A](using gen: K0.Generic[A]): Unify[A] =
    gen.derive(unifyProduct, unifySum)
}