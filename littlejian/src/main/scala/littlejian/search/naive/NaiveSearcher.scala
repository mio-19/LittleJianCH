package littlejian.search.naive

import scala.collection.parallel.immutable.ParVector
import littlejian._
import littlejian.search._

// TODO: use SStream
sealed trait SStream[T] {
  def toStream: Stream[T] = this match {
    case SEmpty() => Stream.empty
    case x: SDelay[T] => x.get.toStream
    case SCons(head, tail) => head #:: tail.toStream
  }
}
final case class SEmpty[T]() extends SStream[T]
final case class SCons[T](head: T, tail: SStream[T]) extends SStream[T]
final class SDelay[T](x: =>SStream[T]) extends SStream[T] {
  def get: SStream[T] = x
}

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
  override def run(state: State, goal: Goal): Stream[State] =
    goal match {
      case goal: GoalBasic => Stream.from(goal.execute(state))
      case GoalDisj(xs) => flatten(xs.map(run(state, _)))
      case GoalConj(xs) => if (xs.isEmpty) Stream(state) else {
        val tail = GoalConj(xs.tail)
        flatten(run(state, xs.head).map(run(_, tail)))
      }
      case goal: GoalDelay => run(state, goal.get)
    }
}