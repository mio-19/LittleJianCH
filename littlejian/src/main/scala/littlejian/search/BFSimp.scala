package littlejian.search

import littlejian._

implicit object BFSimp extends Searcher {
  final class SizedStream[T](val bucket: Vector[T], val thunk: Option[() => SizedStream[T]]) {
  }

  object SizedStream {
    def apply[T](xs: T*): SizedStream[T] = new SizedStream[T](xs.toVector, None)

    def apply[T](thunk: => SizedStream[T]): SizedStream[T] = new SizedStream[T](Vector.empty, Some(() => thunk))
  }
  
  override def run(state: State, goal: Goal): Stream[State] = ???
}
