package littlejian.impl.naive

import scala.collection.parallel.immutable.ParVector

import littlejian._

private def mplus[T](xs: Stream[T], ys: Stream[T]): Stream[T] = xs match {
  case x #:: xs => x #:: mplus(ys, xs)
  case _ => ys
}

private def mplusLazy[T](xs: Stream[T], ys: => Stream[T]): Stream[T] = xs match {
  case x #:: xs => x #:: mplus(ys, xs)
  case _ => ys
}

private def flatten[T](xs: ParVector[Stream[T]]): Stream[T] = xs.fold(Stream.empty)(mplus)

private def flatten[T](xs: Stream[Stream[T]]): Stream[T] = if (xs.isEmpty) Stream.empty else mplusLazy(xs.head, flatten(xs.tail))

implicit class StateOps(self: State) {
  def execute(goal: Goal): Stream[State] = goal match {
    case goal: GoalBasic => Stream.from(goal.execute(self))
    case GoalDisj(xs) => flatten(xs.map(this.execute(_)))
    case GoalConj(xs) => if (xs.isEmpty) Stream(self) else {
      val tail = GoalConj(xs.tail)
      flatten(self.execute(xs.head).map(_.execute(tail)))
    }
    case goal: GoalDelay => this.execute(goal.get)
  }
}
