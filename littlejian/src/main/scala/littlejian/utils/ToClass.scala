package littlejian.utils

import scala.reflect.ClassTag
import scala.runtime.BoxedUnit

implicit class ToClass[T](tag: ClassTag[T]) {
  def toClass: Class[_] =
    if (tag == implicitly[ClassTag[Unit]])
      implicitly[ClassTag[BoxedUnit]].runtimeClass
    else
      tag.runtimeClass
}
