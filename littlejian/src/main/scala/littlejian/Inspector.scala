package littlejian

import scala.annotation.targetName
import scala.language.implicitConversions
import scala.reflect.ClassTag
import littlejian.utils.*
import scala.collection.immutable.HashSet

private val scanRecHistory = new Parameter[HashSet[Any]]

final case class WithInspector[T](x: VarOr[T])(implicit inspector: Inspector[T]) {
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
trait Inspector[T] {
  def inspect(x: T): Seq[WithInspector[_]]

  // None: contains
  // Some(Seq()): not contains
  // Some(Seq(...)): uncertain
  inline final def scanUncertain(x: T, resolver: Any => Any, v: Any): Option[Seq[WithInspector[_]]] = WithInspector(x)(this).scanUncertain(resolver, v)
}

object Inspector {
  // None: contains
  // Some(Seq()): not contains
  // Some(Seq(...)): uncertain
  def scanUncertain[T](x: WithInspector[T], resolver: Any => Any, v: Any): Option[Seq[WithInspector[_]]] = x.scanUncertain(resolver, v)

  def scanUncertain(xs: Vector[WithInspector[_]], resolver: Any => Any, v: Any): Option[Vector[WithInspector[_]]] = traverse(xs.map(_.scanUncertain(resolver, v))).map(_.flatten)
}

trait AtomInspector[T] extends Inspector[T] {
  override def inspect(x: T): Seq[WithInspector[_]] = Seq.empty
}

implicit object I$Var extends AtomInspector[Var[_]]

implicit object I$String extends AtomInspector[String]

implicit object I$Boolean extends AtomInspector[Boolean]

implicit object I$Int extends AtomInspector[Int]

implicit object I$Long extends AtomInspector[Long]

implicit object I$Integer extends AtomInspector[Integer]

implicit object I$Unit extends AtomInspector[Unit]

implicit def I$VarOr[T](implicit inspector: Inspector[T]): Inspector[VarOr[T]] = {
  case v: Var[_] => I$Var.inspect(v)
  case t: T => inspector.inspect(t)
}

@targetName("I$Union_") implicit def I$Union[T, U](implicit tr: => Inspector[T], ur: => Inspector[U], tev: ClassTag[T], uev: ClassTag[U]): Inspector[T | U] = I$Union(tr, ur, tev, uev)
implicit def I$Union[T, U](tr: => Inspector[T], ur: => Inspector[U])(implicit tev: ClassTag[T], uev: ClassTag[U]): Inspector[T | U] = {
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

@targetName("I$Union_") implicit def I$Union[T, U, V](implicit tr: => Inspector[T], ur: => Inspector[U], vr: => Inspector[V], tev: ClassTag[T], uev: ClassTag[U], vev: ClassTag[V]): Inspector[T | U | V] = I$Union(tr, ur, vr, tev, uev, vev)
implicit def I$Union[T, U, V](tr: => Inspector[T], ur: => Inspector[U], vr: => Inspector[V])(implicit tev: ClassTag[T], uev: ClassTag[U], vev: ClassTag[V]): Inspector[T | U | V] = {
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

implicit def I$Product[T, R <: Product1[T]](implicit tr: => Inspector[T]): Inspector[R] = {
  lazy val t = tr
  (x) => Seq(WithInspector(x._1)(t))
}
implicit def I$Product[T, U, R <: Product2[T, U]](implicit tr: => Inspector[T], ur: => Inspector[U]): Inspector[R] = {
  lazy val t = tr
  lazy val u = ur
  (x) => Seq(WithInspector(x._1)(t), WithInspector(x._2)(u))
}
implicit def I$Product[T, U, V, R <: Product3[T, U, V]](implicit tr: => Inspector[T], ur: => Inspector[U], vr: => Inspector[V]): Inspector[R] = {
  lazy val t = tr
  lazy val u = ur
  lazy val v = vr
  (x) => Seq(WithInspector(x._1)(t), WithInspector(x._2)(u), WithInspector(x._3)(v))
}
implicit def I$Produce[A, B, C, D, R <: Product4[A, B, C, D]](implicit ar: => Inspector[A], br: => Inspector[B], cr: => Inspector[C], dr: => Inspector[D]): Inspector[R] = {
  lazy val a = ar
  lazy val b = br
  lazy val c = cr
  lazy val d = dr
  (x) => Seq(WithInspector(x._1)(a), WithInspector(x._2)(b), WithInspector(x._3)(c), WithInspector(x._4)(d))
}