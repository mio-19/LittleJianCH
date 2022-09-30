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
      case _ if self == other => Unifying.success(())
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

def U$Union[T, U](tr: => Unifier[T], ur: => Unifier[U])(implicit tev: ClassTag[T], uev: ClassTag[U]): Unifier[T | U] = {
  lazy val t = tr
  lazy val u = ur
  val tc = tev.runtimeClass
  val uc = uev.runtimeClass
  if (tc == uc) throw new IllegalArgumentException("T == U")
  (x, y) => {
    if (tc.isInstance(x) && tc.isInstance(y)) t.unify(x.asInstanceOf[T], y.asInstanceOf[T])
    else if (uc.isInstance(x) && uc.isInstance(y)) u.unify(x.asInstanceOf[U], y.asInstanceOf[U])
    else Unifying.failure
  }
}

trait EqualUnifier[T] extends Unifier[T] {
  override def concreteUnify(self: T, other: T): Unifying[Unit] = Unifying.guard(self == other)
}

implicit object U$Symbol extends EqualUnifier[Symbol]

implicit object U$String extends EqualUnifier[String]

implicit object U$Unit extends EqualUnifier[Unit]

implicit object U$Int extends EqualUnifier[Int]

implicit object U$Long extends EqualUnifier[Long]

implicit object U$Float extends EqualUnifier[Float]

implicit object U$Double extends EqualUnifier[Double]

implicit object U$Integer extends EqualUnifier[Integer]

implicit object U$Boolean extends EqualUnifier[Boolean]

implicit def U$Product[T, R <: Product1[T]](implicit tr: => Unifier[T]): Unifier[R] = {
  lazy val t = tr
  (x, y) =>
    if (x.getClass != y.getClass) Unifying.failure else t.unify(x._1, y._1)
}

implicit def U$Product[A, B, R <: Product2[A, B]](implicit ar: => Unifier[A], br: => Unifier[B]): Unifier[R] = {
  lazy val a = ar
  lazy val b = br
  (x, y) =>
    if (x.getClass != y.getClass) Unifying.failure else for {
      _ <- a.unify(x._1, y._1)
      _ <- b.unify(x._2, y._2)
    } yield ()
}

implicit def U$Product[A, B, C, R <: Product3[A, B, C]](implicit ar: Unifier[A], br: Unifier[B], cr: Unifier[C]): Unifier[R] = {
  lazy val a = ar
  lazy val b = br
  lazy val c = cr
  (x, y) =>
    if (x.getClass != y.getClass) Unifying.failure else for {
      _ <- a.unify(x._1, y._1)
      _ <- b.unify(x._2, y._2)
      _ <- c.unify(x._3, y._3)
    } yield ()
}