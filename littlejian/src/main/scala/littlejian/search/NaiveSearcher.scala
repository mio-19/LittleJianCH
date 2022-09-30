package littlejian.search

import scala.collection.parallel.immutable.ParVector
import littlejian._
import littlejian.search._
import scala.annotation.tailrec

sealed trait SStream[T] {
  def toStream: Stream[T] = SStream.toStream(this)

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
  def toStream[T](self: SStream[T]): Stream[T] = {
    var result = self
    while (result.isInstanceOf[SDelay[T]]) result = result.asInstanceOf[SDelay[T]].get
    result match {
      case SEmpty() => Stream.empty
      case SCons(head, tail) => head #:: tail.toStream
      case _: SDelay[T] => throw new IllegalStateException("Unreachable state")
    }
  }

  def from[T](xs: IterableOnce[T]): SStream[T] = from(xs.iterator)
  def from[T](xs: Iterator[T]): SStream[T] = if(xs.hasNext) SCons(xs.next, from(xs)) else SEmpty()

  def append[T](xs: IterableOnce[T], tail: SStream[T]): SStream[T] = append(xs.iterator, tail)

  def append[T](xs: Iterator[T], tail: SStream[T]): SStream[T] = if (xs.hasNext) SCons(xs.next, append(xs, tail)) else tail

  def apply[T](x: T*): SStream[T] = SStream.from(x)

  def empty[T] = SEmpty()
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

// TRS2 search strategy, interleaving DFS (DFSi) in "Towards a miniKanren with fair search strategies"
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
      case GoalReadSubst(f) => SDelay(runs(state, f(state.eq.subst)))
      case goal: GoalDelay => SDelay(runs(state, goal.get))
    }
}