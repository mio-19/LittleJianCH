package littlejian

import scala.language.implicitConversions

// Monad
type Unifying[T] = Subst => Option[(Subst, T)]

object Unifying {
  def success[T](x: T): Unifying[T] = s => Some(s, x)

  def failure[T]: Unifying[T] = s => None

  def guard(x: Boolean): Unifying[Unit] = if (x) success(()) else failure
}

implicit class UnifyingOps[T](self: Unifying[T]) {
  def map[U](f: T => U): Unifying[U] = subst => self(subst) match {
    case Some((s, x)) => Some((s, f(x)))
    case None => None
  }

  def flatMap[U](f: T => Unifying[U]): Unifying[U] = subst => self(subst) match {
    case Some((s, x)) => f(x)(s)
    case None => None
  }
}

trait Unifier[T] {
  final implicit val thisUnifier: Unifier[T] = this

  final def unify(self: VarOr[T], other: VarOr[T]): Unifying[Unit] = for {
    self <- Subst.walk(self)
    other <- Subst.walk(other)
    _ <- (self, other) match {
      case (self: Var[_], _) => Subst.addEntry(self.asInstanceOf[Var[T]], other)
      case (_, other: Var[_]) => Subst.addEntry(other.asInstanceOf[Var[T]], self)
      case _ => concreteUnify(self.asInstanceOf[T], other.asInstanceOf[T])
    }
  } yield ()

  def concreteUnify(self: T, other: T): Unifying[Unit]
}

implicit class InfixUnify[T](self: VarOr[T])(implicit unifier: Unifier[T]) {
  def unify(other: VarOr[T]): Unifying[Unit] = unifier.unify(self, other)
}

implicit def U$VarOr[T](implicit unifier: Unifier[T]): Unifier[VarOr[T]] = (x, y) => unifier.unify(x, y)


import scala.reflect.ClassTag

def U$Union[T, U](t: Unifier[T], u: Unifier[U])(implicit tev: ClassTag[T], uev: ClassTag[U]): Unifier[T | U] = {
  if (tev.runtimeClass == uev.runtimeClass) throw new IllegalArgumentException("T == U")
  (x, y) =>
    (x, y) match {
      case (x: T, y: T) => t.unify(x, y)
      case (x: U, y: U) => u.unify(x, y)
      case (_: T, _: U) | (_: U, _: T) => Unifying.failure
      case (_, _) => throw new IllegalStateException("Unexpected")
    }
}

trait EqualUnifier[T] extends Unifier[T] {
  override def concreteUnify(self: T, other: T): Unifying[Unit] = Unifying.guard(self == other)
}

implicit object U$Symbol extends EqualUnifier[Symbol]

implicit object U$String extends EqualUnifier[String]

implicit object U$Unit extends EqualUnifier[Unit]

implicit object U$Int extends EqualUnifier[Int]

implicit object U$Integer extends EqualUnifier[Integer]

implicit object U$Boolean extends EqualUnifier[Boolean]