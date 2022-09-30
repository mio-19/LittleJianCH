package littlejian

import scala.annotation.targetName
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.collection.parallel.immutable.ParVector
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
  final def scanUncertain(x: T, resolver: Any => Any, v: Any): Option[Seq[WithInspector[_]]] = WithInspector(x)(this).scanUncertain(resolver, v)
}

object Inspector {
  // None: contains
  // Some(Seq()): not contains
  // Some(Seq(...)): uncertain
  def scanUncertain[T](x: WithInspector[T], resolver: Any => Any, v: Any): Option[Seq[WithInspector[_]]] = x.scanUncertain(resolver, v)

  def scanUncertain(xs: ParVector[WithInspector[_]], resolver: Any => Any, v: Any): Option[ParVector[WithInspector[_]]] = traverse(xs.map(_.scanUncertain(resolver, v))).map(_.flatten)
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