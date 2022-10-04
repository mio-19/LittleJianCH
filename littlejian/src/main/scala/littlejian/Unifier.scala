package littlejian

import littlejian.utils._

import scala.annotation.{tailrec, targetName}
import scala.language.implicitConversions

// Monad
type Unifying[T] = StateOption[(Subst, SubstPatch), T]

object Unifying {
  def success[T](x: T): Unifying[T] = StateOption.success(x)

  def failure[T]: Unifying[T] = StateOption.failure

  def guard(x: Boolean): Unifying[Unit] = StateOption.guard(x)
}

implicit class UnifyingOps[T](self: Unifying[T]) {
  def getSubst(subst: Subst): Option[Subst] = self.run((subst, SubstPatch.empty)).map(_._1._1)

  def getSubstWithPatch(subst: Subst): Option[(Subst, SubstPatch)] = self.run((subst, SubstPatch.empty)).map(_._1)

  def getSubstPatch(subst: Subst): Option[SubstPatch] = self.run((subst, SubstPatch.empty)).map(_._1._2)

  def map[U](f: T => U): Unifying[U] = self.map(f)

  def flatMap[U](f: T => Unifying[U]): Unifying[U] = self.flatMap(f)
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

@targetName("U$Union_") def U$Union[T, U](implicit tr: => Unifier[T], ur: => Unifier[U], tev: ClassTag[T], uev: ClassTag[U]): Unifier[T | U] = U$Union(tr, ur)(tev, uev)

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

@targetName("U$Union_") def U$Union[T, U, V](implicit tr: => Unifier[T], ur: => Unifier[U], vr: => Unifier[V], tev: ClassTag[T], uev: ClassTag[U], vev: ClassTag[V]): Unifier[T | U | V] = U$Union(tr, ur, vr)(tev, uev, vev)
def U$Union[T, U, V](tr: => Unifier[T], ur: => Unifier[U], vr: => Unifier[V])(implicit tev: ClassTag[T], uev: ClassTag[U], vev: ClassTag[V]): Unifier[T | U | V] = {
  lazy val t = tr
  lazy val u = ur
  lazy val v = vr
  val tc = tev.runtimeClass
  val uc = uev.runtimeClass
  val vc = vev.runtimeClass
  if (tc == uc || tc == vc || uc == vc) throw new IllegalArgumentException("T == U || T == V || U == V")
  (x, y) => {
    if (tc.isInstance(x) && tc.isInstance(y)) t.unify(x.asInstanceOf[T], y.asInstanceOf[T])
    else if (uc.isInstance(x) && uc.isInstance(y)) u.unify(x.asInstanceOf[U], y.asInstanceOf[U])
    else if (vc.isInstance(x) && vc.isInstance(y)) v.unify(x.asInstanceOf[V], y.asInstanceOf[V])
    else Unifying.failure
  }
}
@targetName("U$Union_") def U$Union[T, U, V, W](implicit tr: => Unifier[T], ur: => Unifier[U], vr: => Unifier[V], wr: => Unifier[W], tev: ClassTag[T], uev: ClassTag[U], vev: ClassTag[V], wev: ClassTag[W]): Unifier[T | U | V | W] = U$Union(tr, ur, vr, wr)(tev, uev, vev, wev)
def U$Union[T, U, V, W](tr: => Unifier[T], ur: => Unifier[U], vr: => Unifier[V], wr: => Unifier[W])(implicit tev: ClassTag[T], uev: ClassTag[U], vev: ClassTag[V], wev: ClassTag[W]): Unifier[T | U | V | W] = {
  lazy val t = tr
  lazy val u = ur
  lazy val v = vr
  lazy val w = wr
  val tc = tev.runtimeClass
  val uc = uev.runtimeClass
  val vc = vev.runtimeClass
  val wc = wev.runtimeClass
  if (tc == uc || tc == vc || tc == wc || uc == vc || uc == wc || vc == wc) throw new IllegalArgumentException("T == U || T == V || T == W || U == V || U == W || V == W")
  (x, y) => {
    if (tc.isInstance(x) && tc.isInstance(y)) t.unify(x.asInstanceOf[T], y.asInstanceOf[T])
    else if (uc.isInstance(x) && uc.isInstance(y)) u.unify(x.asInstanceOf[U], y.asInstanceOf[U])
    else if (vc.isInstance(x) && vc.isInstance(y)) v.unify(x.asInstanceOf[V], y.asInstanceOf[V])
    else if (wc.isInstance(x) && wc.isInstance(y)) w.unify(x.asInstanceOf[W], y.asInstanceOf[W])
    else Unifying.failure
  }
}

trait EqualUnifier[T] extends Unifier[T] {
  override def concreteUnify(self: T, other: T): Unifying[Unit] = Unifying.guard(self == other)
}

def equalUnifier[T]: Unifier[T] = new EqualUnifier[T] {}

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
implicit def U$Product[A, B, C, D, R <: Product4[A, B, C, D]](implicit ar: Unifier[A], br: Unifier[B], cr: Unifier[C], dr: Unifier[D]): Unifier[R] = {
  lazy val a = ar
  lazy val b = br
  lazy val c = cr
  lazy val d = dr
  (x, y) =>
    if (x.getClass != y.getClass) Unifying.failure else for {
      _ <- a.unify(x._1, y._1)
      _ <- b.unify(x._2, y._2)
      _ <- c.unify(x._3, y._3)
      _ <- d.unify(x._4, y._4)
    } yield ()
}
implicit def U$Product[A, B, C, D, E, R <: Product5[A, B, C, D, E]](implicit ar: Unifier[A], br: Unifier[B], cr: Unifier[C], dr: Unifier[D], er: Unifier[E]): Unifier[R] = {
  lazy val a = ar
  lazy val b = br
  lazy val c = cr
  lazy val d = dr
  lazy val e = er
  (x, y) =>
    if (x.getClass != y.getClass) Unifying.failure else for {
      _ <- a.unify(x._1, y._1)
      _ <- b.unify(x._2, y._2)
      _ <- c.unify(x._3, y._3)
      _ <- d.unify(x._4, y._4)
      _ <- e.unify(x._5, y._5)
    } yield ()
}