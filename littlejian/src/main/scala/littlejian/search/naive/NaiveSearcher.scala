package littlejian.search.naive

import scala.collection.parallel.immutable.ParVector
import littlejian._
import littlejian.search._

sealed trait SStream[T] {
  def toStream: Stream[T] = this match {
    case SEmpty() => Stream.empty
    case x: SDelay[T] => x.get.toStream
    case SCons(head, tail) => head #:: tail.toStream
  }

  def map[U](f: T => U): SStream[U] = this match {
    case SEmpty() => SEmpty()
    case x: SDelay[T] => SDelay(x.get.map(f))
    case SCons(head, tail) => SCons(f(head), tail.map(f))
  }
}

final case class SEmpty[T]() extends SStream[T]

final case class SCons[T](head: T, tail: SStream[T]) extends SStream[T]

final class SDelay[T](x: => SStream[T]) extends SStream[T] {
  def get: SStream[T] = x
}

object SStream {
  def from[T](xs: Iterable[T]): SStream[T] = if (xs.isEmpty) SEmpty() else SCons(xs.head, SStream.from(xs.tail))

  def apply[T](x: T*): SStream[T] = SStream.from(x)
}

def mplus[T](xs: SStream[T], ys: SStream[T]): SStream[T] = xs match {
  case SCons(x, xs) => SCons(x, mplus(ys, xs))
  case xs: SDelay[T] => SDelay(mplus(ys, xs.get))
  case SEmpty() => ys
}

def flatten[T](xs: ParVector[SStream[T]]): SStream[T] = xs.fold(SEmpty())(mplus)

def flatten[T](xs: SStream[SStream[T]]): SStream[T] = xs match {
  case SCons(x, xs) => mplus(x, flatten(xs))
  case SEmpty() => SEmpty()
  case xs: SDelay[SStream[T]] => SDelay(flatten(xs.get))
}

implicit object NaiveSearcher extends Searcher {
  override def run(state: State, goal: Goal): Stream[State] = runs(state, goal).toStream
  def runs(state: State, goal: Goal): SStream[State] =
    goal match {
      case goal: GoalBasic => SStream.from(goal.execute(state))
      case GoalDisj(xs) => SDelay(flatten(xs.map(runs(state, _))))
      case GoalConj(xs) => if (xs.isEmpty) SStream(state) else {
        val tail = GoalConj(xs.tail)
        SDelay(flatten(runs(state, xs.head).map(runs(_, tail))))
      }
      case goal: GoalDelay => SDelay(runs(state, goal.get))
    }
}