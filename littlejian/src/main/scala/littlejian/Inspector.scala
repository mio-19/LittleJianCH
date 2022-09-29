package littlejian

import scala.annotation.targetName
import scala.reflect.ClassTag

final case class WithInspector[T](x: T)(implicit inspector: Inspector[T])

// for GoalAbsent usages
trait Inspector[T] {
  def inspect(x: T): Seq[WithInspector[_]]
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

@targetName("I$Union2") implicit def I$Union[T, U](t: Inspector[T], u: Inspector[U])(implicit tev: ClassTag[T], uev: ClassTag[U]): Inspector[T | U] = I$Union(t, u, tev, uev)

implicit def I$Union[T, U](implicit t: Inspector[T], u: Inspector[U], tev: ClassTag[T], uev: ClassTag[U]): Inspector[T | U] = {
  val tc = tev.runtimeClass
  val uc = uev.runtimeClass
  if (tc == uc) throw new IllegalArgumentException("T == U")
  (x) => {
    if(tc.isInstance(x)) t.inspect(x.asInstanceOf[T])
    else u.inspect(x.asInstanceOf[U])
  }
}