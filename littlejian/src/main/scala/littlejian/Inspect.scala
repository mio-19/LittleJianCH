package littlejian

import scala.annotation.targetName
import scala.language.implicitConversions
import scala.reflect.ClassTag
import littlejian.utils.*
import scala.collection.immutable.HashSet

private val scanRecHistory = new Parameter[HashSet[Any]]

final case class WithInspector[T](x: VarOr[T])(implicit inspector: Inspect[T]) {
  // None: contains
  // Some(Seq()): not contains
  // Some(Seq(...)): uncertain
  final def scanUncertain(resolver: Any => Any, v: Any): Option[Seq[WithInspector[_]]] = {
    val todo = resolver(x).asInstanceOf[VarOr[T]]
    val history = scanRecHistory.get.getOrElse(HashSet.empty)
    if (history.contains(todo)) return Some(Seq())
    if (todo.isInstanceOf[Var[_]])
      Some(Seq(this))
    else if (todo == v) None
    else scanRecHistory.callWith(history.incl(todo)) {
      traverse(inspector.inspect(todo.asInstanceOf[T]).map(_.scanUncertain(resolver, v))).map(_.flatten)
    }
  }
}

// for GoalAbsent usages
trait Inspect[T] {
  def inspect(x: T): Seq[WithInspector[_]]

  // None: contains
  // Some(Seq()): not contains
  // Some(Seq(...)): uncertain
  inline final def scanUncertain(x: T, resolver: Any => Any, v: Any): Option[Seq[WithInspector[_]]] = WithInspector(x)(this).scanUncertain(resolver, v)
}
/* // TODO: rewrite Inspect
trait Internaler {
  def apply[T](x: T)(implicit inspect: Internal[T]): Boolean
}

trait Internal[T] {
  def contains(rec: Internaler)(self: T, x: Any): Boolean
}

private val containsRecHistory = new Parameter[HashSet[Any]]

implicit class InternalOps[T](self: Internal[T]) {
  def runContains(walker: Any => Any, v: VarOr[T], x: Any): Boolean = {
    if (v == x) return true
    val value: VarOr[T] = walker(v).asInstanceOf
    if (value == x) return true
    val history = containsRecHistory.get.getOrElse(HashSet.empty)
    if (history.contains(value)) return false
    if (value.isInstanceOf[Var[_]]) return false
    containsRecHistory.callWith(history.incl(value)) {
      self.contains(???)(value.asInstanceOf, x)
    }
  }
}
*/

object Inspect {
  // None: contains
  // Some(Seq()): not contains
  // Some(Seq(...)): uncertain
  def scanUncertain[T](x: WithInspector[T], resolver: Any => Any, v: Any): Option[Seq[WithInspector[_]]] = x.scanUncertain(resolver, v)

  def scanUncertain(xs: Vector[WithInspector[_]], resolver: Any => Any, v: Any): Option[Vector[WithInspector[_]]] = traverse(xs.map(_.scanUncertain(resolver, v))).map(_.flatten)


  import shapeless3.deriving.*

  given inspectSum[A] (using inst: K0.CoproductInstances[Inspect, A]): Inspect[A] with
    def inspect(x: A): Seq[WithInspector[_]] = inst.fold(x)(
      [t] => (i: Inspect[t], t0: t) => Seq(WithInspector(t0)(i))
    )

  given inspectProduct[A] (using inst: K0.ProductInstances[Inspect, A]): Inspect[A] with
    def inspect(x: A): Seq[WithInspector[_]] = inst.foldLeft(x)(Seq.empty: Seq[WithInspector[_]])(
      [t] => (acc: Seq[WithInspector[_]], i: Inspect[t], t0: t) =>
        WithInspector(t0)(i) +: acc
    )

  inline def derived[A](using gen: K0.Generic[A]): Inspect[A] =
    gen.derive(inspectProduct, inspectSum)
}

trait AtomInspect[T] extends Inspect[T] {
  override def inspect(x: T): Seq[WithInspector[_]] = Seq.empty
}

implicit object I$Var extends AtomInspect[Var[_]]

implicit object I$String extends AtomInspect[String]

implicit object I$BigDecimal extends AtomInspect[BigDecimal]

implicit object I$Boolean extends AtomInspect[Boolean]

implicit object I$Short extends AtomInspect[Short]

implicit object I$Int extends AtomInspect[Int]

implicit object I$Long extends AtomInspect[Long]

implicit object I$Float extends AtomInspect[Float]

implicit object I$Double extends AtomInspect[Double]

implicit object I$Integer extends AtomInspect[Integer]

implicit object I$Unit extends AtomInspect[Unit]

implicit def I$VarOr[T](implicit inspector: Inspect[T]): Inspect[VarOr[T]] = {
  case v: Var[_] => I$Var.inspect(v)
  case t => inspector.inspect(t.asInstanceOf)
}

//@targetName("I$Union_") implicit def I$Union[T, U](implicit tr: => Inspect[T], ur: => Inspect[U], tev: ClassTag[T], uev: ClassTag[U]): Inspect[T | U] = I$Union(tr, ur)(tev, uev)
implicit def I$Union[T, U](tr: => Inspect[T], ur: => Inspect[U])(implicit tev: ClassTag[T], uev: ClassTag[U]): Inspect[T | U] = {
  lazy val t = tr
  lazy val u = ur
  val tc = tev.runtimeClass
  val uc = uev.runtimeClass
  if (tc == uc) throw new IllegalArgumentException("T == U")
  (x) => {
    if (tc.isInstance(x)) t.inspect(x.asInstanceOf[T])
    else u.inspect(x.asInstanceOf[U])
  }
}

//@targetName("I$Union_") implicit def I$Union[T, U, V](implicit tr: => Inspect[T], ur: => Inspect[U], vr: => Inspect[V], tev: ClassTag[T], uev: ClassTag[U], vev: ClassTag[V]): Inspect[T | U | V] = I$Union(tr, ur, vr)(tev, uev, vev)
implicit def I$Union[T, U, V](tr: => Inspect[T], ur: => Inspect[U], vr: => Inspect[V])(implicit tev: ClassTag[T], uev: ClassTag[U], vev: ClassTag[V]): Inspect[T | U | V] = {
  lazy val t = tr
  lazy val u = ur
  lazy val v = vr
  val tc = tev.runtimeClass
  val uc = uev.runtimeClass
  val vc = vev.runtimeClass
  if (tc == uc || tc == vc || uc == vc) throw new IllegalArgumentException("T == U or T == V or U == V")
  (x) => {
    if (tc.isInstance(x)) t.inspect(x.asInstanceOf[T])
    else if (uc.isInstance(x)) u.inspect(x.asInstanceOf[U])
    else v.inspect(x.asInstanceOf[V])
  }
}

//@targetName("I$Union_") implicit def I$Union[T, U, V, W](implicit tr: => Inspect[T], ur: => Inspect[U], vr: => Inspect[V], wr: => Inspect[W], tev: ClassTag[T], uev: ClassTag[U], vev: ClassTag[V], wev: ClassTag[W]): Inspect[T | U | V | W] = I$Union(tr, ur, vr, wr)(tev, uev, vev, wev)
implicit def I$Union[T, U, V, W](tr: => Inspect[T], ur: => Inspect[U], vr: => Inspect[V], wr: => Inspect[W])(implicit tev: ClassTag[T], uev: ClassTag[U], vev: ClassTag[V], wev: ClassTag[W]): Inspect[T | U | V | W] = {
  lazy val t = tr
  lazy val u = ur
  lazy val v = vr
  lazy val w = wr
  val tc = tev.runtimeClass
  val uc = uev.runtimeClass
  val vc = vev.runtimeClass
  val wc = wev.runtimeClass
  if (tc == uc || tc == vc || tc == wc || uc == vc || uc == wc || vc == wc) throw new IllegalArgumentException("T == U or T == V or T == W or U == V or U == W or V == W")
  (x) => {
    if (tc.isInstance(x)) t.inspect(x.asInstanceOf[T])
    else if (uc.isInstance(x)) u.inspect(x.asInstanceOf[U])
    else if (vc.isInstance(x)) v.inspect(x.asInstanceOf[V])
    else w.inspect(x.asInstanceOf[W])
  }
}

//@targetName("I$Union_") implicit def I$Union[T, U, V, W, X](implicit tr: => Inspect[T], ur: => Inspect[U], vr: => Inspect[V], wr: => Inspect[W], xr: => Inspect[X], tev: ClassTag[T], uev: ClassTag[U], vev: ClassTag[V], wev: ClassTag[W], xev: ClassTag[X]): Inspect[T | U | V | W | X] = I$Union(tr, ur, vr, wr, xr)(tev, uev, vev, wev, xev)
implicit def I$Union[T, U, V, W, X](tr: => Inspect[T], ur: => Inspect[U], vr: => Inspect[V], wr: => Inspect[W], xr: => Inspect[X])(implicit tev: ClassTag[T], uev: ClassTag[U], vev: ClassTag[V], wev: ClassTag[W], xev: ClassTag[X]): Inspect[T | U | V | W | X] = {
  lazy val t = tr
  lazy val u = ur
  lazy val v = vr
  lazy val w = wr
  lazy val x = xr
  val tc = tev.runtimeClass
  val uc = uev.runtimeClass
  val vc = vev.runtimeClass
  val wc = wev.runtimeClass
  val xc = xev.runtimeClass
  if (tc == uc || tc == vc || tc == wc || tc == xc || uc == vc || uc == wc || uc == xc || vc == wc || vc == xc || wc == xc) throw new IllegalArgumentException("T == U or T == V or T == W or T == X or U == V or U == W or U == X or V == W or V == X or W == X")
  (arg) => {
    if (tc.isInstance(arg)) t.inspect(arg.asInstanceOf[T])
    else if (uc.isInstance(arg)) u.inspect(arg.asInstanceOf[U])
    else if (vc.isInstance(arg)) v.inspect(arg.asInstanceOf[V])
    else if (wc.isInstance(arg)) w.inspect(arg.asInstanceOf[W])
    else x.inspect(arg.asInstanceOf[X])
  }
}

//@targetName("I$Union_") implicit def I$Union[T, U, V, W, X, Y](implicit tr: => Inspect[T], ur: => Inspect[U], vr: => Inspect[V], wr: => Inspect[W], xr: => Inspect[X], yr: => Inspect[Y], tev: ClassTag[T], uev: ClassTag[U], vev: ClassTag[V], wev: ClassTag[W], xev: ClassTag[X], yev: ClassTag[Y]): Inspect[T | U | V | W | X | Y] = I$Union(tr, ur, vr, wr, xr, yr)(tev, uev, vev, wev, xev, yev)
implicit def I$Union[T, U, V, W, X, Y](tr: => Inspect[T], ur: => Inspect[U], vr: => Inspect[V], wr: => Inspect[W], xr: => Inspect[X], yr: => Inspect[Y])(implicit tev: ClassTag[T], uev: ClassTag[U], vev: ClassTag[V], wev: ClassTag[W], xev: ClassTag[X], yev: ClassTag[Y]): Inspect[T | U | V | W | X | Y] = {
  lazy val t = tr
  lazy val u = ur
  lazy val v = vr
  lazy val w = wr
  lazy val x = xr
  lazy val y = yr
  val tc = tev.runtimeClass
  val uc = uev.runtimeClass
  val vc = vev.runtimeClass
  val wc = wev.runtimeClass
  val xc = xev.runtimeClass
  val yc = yev.runtimeClass
  if (tc == uc || tc == vc || tc == wc || tc == xc || tc == yc || uc == vc || uc == wc || uc == xc || uc == yc || vc == wc || vc == xc || vc == yc || wc == xc || wc == yc || xc == yc) throw new IllegalArgumentException("T == U or T == V or T == W or T == X or T == Y or U == V or U == W or U == X or U == Y or V == W or V == X or V == Y or W == X or W == Y or X == Y")
  (arg) => {
    if (tc.isInstance(arg)) t.inspect(arg.asInstanceOf[T])
    else if (uc.isInstance(arg)) u.inspect(arg.asInstanceOf[U])
    else if (vc.isInstance(arg)) v.inspect(arg.asInstanceOf[V])
    else if (wc.isInstance(arg)) w.inspect(arg.asInstanceOf[W])
    else if (xc.isInstance(arg)) x.inspect(arg.asInstanceOf[X])
    else y.inspect(arg.asInstanceOf[Y])
  }
}

//@targetName("I$Union_") implicit def I$Union[T, U, V, W, X, Y, Z](implicit tr: => Inspect[T], ur: => Inspect[U], vr: => Inspect[V], wr: => Inspect[W], xr: => Inspect[X], yr: => Inspect[Y], zr: => Inspect[Z], tev: ClassTag[T], uev: ClassTag[U], vev: ClassTag[V], wev: ClassTag[W], xev: ClassTag[X], yev: ClassTag[Y], zev: ClassTag[Z]): Inspect[T | U | V | W | X | Y | Z] = I$Union(tr, ur, vr, wr, xr, yr, zr)(tev, uev, vev, wev, xev, yev, zev)
implicit def I$Union[T, U, V, W, X, Y, Z](tr: => Inspect[T], ur: => Inspect[U], vr: => Inspect[V], wr: => Inspect[W], xr: => Inspect[X], yr: => Inspect[Y], zr: => Inspect[Z])(implicit tev: ClassTag[T], uev: ClassTag[U], vev: ClassTag[V], wev: ClassTag[W], xev: ClassTag[X], yev: ClassTag[Y], zev: ClassTag[Z]): Inspect[T | U | V | W | X | Y | Z] = {
  lazy val t = tr
  lazy val u = ur
  lazy val v = vr
  lazy val w = wr
  lazy val x = xr
  lazy val y = yr
  lazy val z = zr
  val tc = tev.runtimeClass
  val uc = uev.runtimeClass
  val vc = vev.runtimeClass
  val wc = wev.runtimeClass
  val xc = xev.runtimeClass
  val yc = yev.runtimeClass
  val zc = zev.runtimeClass
  if (tc == uc || tc == vc || tc == wc || tc == xc || tc == yc || tc == zc || uc == vc || uc == wc || uc == xc || uc == yc || uc == zc || vc == wc || vc == xc || vc == yc || vc == zc || wc == xc || wc == yc || wc == zc || xc == yc || xc == zc || yc == zc) throw new IllegalArgumentException("T == U or T == V or T == W or T == X or T == Y or T == Z or U == V or U == W or U == X or U == Y or U == Z or V == W or V == X or V == Y or V == Z or W == X or W == Y or W == Z or X == Y or X == Z or Y == Z")
  (arg) => {
    if (tc.isInstance(arg)) t.inspect(arg.asInstanceOf[T])
    else if (uc.isInstance(arg)) u.inspect(arg.asInstanceOf[U])
    else if (vc.isInstance(arg)) v.inspect(arg.asInstanceOf[V])
    else if (wc.isInstance(arg)) w.inspect(arg.asInstanceOf[W])
    else if (xc.isInstance(arg)) x.inspect(arg.asInstanceOf[X])
    else if (yc.isInstance(arg)) y.inspect(arg.asInstanceOf[Y])
    else z.inspect(arg.asInstanceOf[Z])
  }
}

implicit def I$Product[T, R <: Product1[T]](implicit tr: => Inspect[T]): Inspect[R] = {
  lazy val t = tr
  (x) => Seq(WithInspector(x._1)(t))
}
implicit def I$Product[T, U, R <: Product2[T, U]](implicit tr: => Inspect[T], ur: => Inspect[U]): Inspect[R] = {
  lazy val t = tr
  lazy val u = ur
  (x) => Seq(WithInspector(x._1)(t), WithInspector(x._2)(u))
}
implicit def I$Product[T, U, V, R <: Product3[T, U, V]](implicit tr: => Inspect[T], ur: => Inspect[U], vr: => Inspect[V]): Inspect[R] = {
  lazy val t = tr
  lazy val u = ur
  lazy val v = vr
  (x) => Seq(WithInspector(x._1)(t), WithInspector(x._2)(u), WithInspector(x._3)(v))
}
implicit def I$Produce[A, B, C, D, R <: Product4[A, B, C, D]](implicit ar: => Inspect[A], br: => Inspect[B], cr: => Inspect[C], dr: => Inspect[D]): Inspect[R] = {
  lazy val a = ar
  lazy val b = br
  lazy val c = cr
  lazy val d = dr
  (x) => Seq(WithInspector(x._1)(a), WithInspector(x._2)(b), WithInspector(x._3)(c), WithInspector(x._4)(d))
}