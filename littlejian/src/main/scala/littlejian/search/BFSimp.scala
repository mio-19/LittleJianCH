package littlejian.search

import littlejian._

implicit object BFSimp extends Searcher {
  final class SizedStream[T](val bucket: Vector[T], val thunk: Option[() => SizedStream[T]]) {
    def appendFair(other: SizedStream[T]): SizedStream[T] = {
      new SizedStream(bucket ++ other.bucket, (thunk, other.thunk) match {
        case (None, xs) => xs
        case (xs, None) => xs
        case (Some(xs), Some(ys)) => Some(() => xs().appendFair(ys()))
      })
    }

    def elim[U](default: => SizedStream[U], kf: (T, SizedStream[T]) => SizedStream[U]): SizedStream[U] = {
      if (bucket.isEmpty) {
        thunk match {
          case None => default
          case Some(xs) => SizedStream(xs().elim(default, kf))
        }
      } else {
        kf(bucket.head, new SizedStream(bucket.tail, thunk))
      }
    }
  }

  object SizedStream {
    def apply[T](xs: T*): SizedStream[T] = new SizedStream[T](xs.toVector, None)

    def apply[T](thunk: => SizedStream[T]): SizedStream[T] = new SizedStream[T](Vector.empty, Some(() => thunk))
  }

  override def run(state: State, goal: Goal): Stream[State] = ???
}
