package littlejian.unifier

import littlejian._
import scala.language.implicitConversions

implicit def U$Tuple2[T1, T2](implicit u1: Unifier[T1], u2: Unifier[T2]): Unifier[(T1, T2)] = {
  case ((x1, x2), (y1, y2)) => for {
    _ <- x1.unify(y1)
    _ <- x2.unify(y2)
  } yield ()
}
