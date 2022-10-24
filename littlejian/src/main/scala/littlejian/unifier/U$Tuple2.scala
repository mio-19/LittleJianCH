package littlejian.unifier

import littlejian._
import scala.language.implicitConversions

implicit def U$Tuple2[T1, T2](implicit u1: Unify[T1], u2: Unify[T2]): Unify[(T1, T2)] = {
  case ((x1, x2), (y1, y2)) => for {
    _ <- x1.unify(y1)
    _ <- x2.unify(y2)
  } yield ()
}
