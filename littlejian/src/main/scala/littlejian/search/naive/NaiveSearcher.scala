package littlejian.search.naive

import scala.collection.parallel.immutable.ParVector
import littlejian._
import littlejian.search._

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

implicit object NaiveSearcher extends Searcher {
  override def run(state: State, goal: Goal): Stream[State] = goal match {
    case goal: GoalBasic => Stream.from(goal.execute(state))
    case GoalDisj(xs) => flatten(xs.map(run(state, _)))
    case GoalConj(xs) => if (xs.isEmpty) Stream(state) else {
      val tail = GoalConj(xs.tail)
      flatten(run(state, xs.head).map(run(_, tail)))
    }
    case goal: GoalDelay => run(state, goal.get)
  }
}