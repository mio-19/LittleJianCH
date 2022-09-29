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
  // todo: catch recursion scan
  final def scanUncertain(resolver: Any => Any, v: Any): Option[Seq[WithInspector[_]]] = {
    val todo = resolver(x).asInstanceOf[VarOr[T]]
    val history = scanRecHistory.get.getOrElse(HashSet.empty)
    if(history.contains(todo)) return Some(Seq())
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

@targetName("I$Union2") implicit def I$Union[T, U](t: Inspector[T], u: Inspector[U])(implicit tev: ClassTag[T], uev: ClassTag[U]): Inspector[T | U] = I$Union(t, u, tev, uev)

implicit def I$Union[T, U](implicit t: Inspector[T], u: Inspector[U], tev: ClassTag[T], uev: ClassTag[U]): Inspector[T | U] = {
  val tc = tev.runtimeClass
  val uc = uev.runtimeClass
  if (tc == uc) throw new IllegalArgumentException("T == U")
  (x) => {
    if (tc.isInstance(x)) t.inspect(x.asInstanceOf[T])
    else u.inspect(x.asInstanceOf[U])
  }
}