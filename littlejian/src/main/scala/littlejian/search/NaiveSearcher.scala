package littlejian.search

import scala.collection.parallel.immutable.ParVector
import littlejian.*
import littlejian.search.*
import collection.parallel.CollectionConverters._

import scala.annotation.tailrec

sealed trait SStream[T] {
  def toStream: Stream[T] = SStream.toStream(this)

  def map[U](f: T => U): SStream[U] = this match {
    case SEmpty() => SEmpty()
    case x: SDelay[_] => SDelay(x.get.map(f))
    case SCons(head, tail) => SCons(f(head), tail.map(f))
  }

  def take1FlatMap[U](default: => SStream[U], f: T => SStream[U]): SStream[U] = this match {
    case SEmpty() => SEmpty()
    case x: SDelay[_] => SDelay(x.get.take1FlatMap(default, f))
    case SCons(head, _) => f(head)
  }

  def caseOnEmpty[U](default: => SStream[U], f: SStream[T] => SStream[U]): SStream[U] = this match {
    case SEmpty() => default
    case x: SDelay[_] => SDelay(x.get.caseOnEmpty(default, f))
    case SCons(_, _) => f(this)
  }

  def reduce(n: Int): SStream[T] = {
    var times = n
    var result = this
    while (times > 0 && result.isInstanceOf[SDelay[_]]) {
      result = result.asInstanceOf[SDelay[T]].get
      times -= 1
    }
    result
  }
  // TODO
  def parMapImmediate(f: T => T): SStream[T] = this
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

  def from[T](xs: Iterator[T]): SStream[T] = if (xs.hasNext) SCons(xs.next, from(xs)) else SEmpty()

  def append[T](xs: IterableOnce[T], tail: SStream[T]): SStream[T] = append(xs.iterator, tail)

  def append[T](xs: Iterator[T], tail: SStream[T]): SStream[T] = if (xs.hasNext) SCons(xs.next, append(xs, tail)) else tail

  def apply[T](x: T*): SStream[T] = SStream.from(x)

  def empty[T]: SStream[T] = SEmpty()
}

def mplus[T](xs: SStream[T], ys: SStream[T]): SStream[T] = xs match {
  case SCons(x, xs) => SCons(x, mplus(ys, xs))
  case xs: SDelay[T] => SDelay(mplus(ys, xs.get))
  case SEmpty() => ys
}

// TRS2 search strategy, interleaving DFS (DFSi) in "Towards a miniKanren with fair search strategies"
def unfairFlatten[T](xs: ParVector[SStream[T]]): SStream[T] = if(xs.isEmpty) SEmpty() else mplus(xs.head, unfairFlatten(xs.tail))

// balanced interleaving depth-first search (DFSbi)
def fairFlatten[T](xs: ParVector[SStream[T]]): SStream[T] =
  if(xs.isEmpty)
    SEmpty()
  else if(xs.size == 1) xs.head
  else if(xs.size == 2) mplus(xs(0), xs(1))
  else {
    val (left, right) = xs.splitAt(xs.size / 2)
    mplus(fairFlatten(left), fairFlatten(right))
  }
def fairFlatten[T](xs: Vector[SStream[T]]): SStream[T] =
    if (xs.isEmpty)
      SEmpty()
    else if (xs.size == 1) xs.head
    else if (xs.size == 2) mplus(xs(0), xs(1))
    else {
      val (left, right) = xs.splitAt(xs.size / 2)
      mplus(fairFlatten(left), fairFlatten(right))
    }

def unfairFlatten[T](xs: SStream[SStream[T]]): SStream[T] = xs match {
  case SCons(x, xs) => mplus(x, unfairFlatten(xs))
  case SEmpty() => SEmpty()
  case xs: SDelay[SStream[T]] => SDelay(unfairFlatten(xs.get))
}

def fairFlatten[T](xs: SStream[SStream[T]]): SStream[T] = doFairFlatten(Vector.empty, xs, Vector.empty)
private def doFairFlatten[T](left: Vector[SStream[T]], xs: SStream[SStream[T]], right: Vector[SStream[T]]): SStream[T] = (left, xs, right) match {
  case (left, SCons(x, xs), right) => doFairFlatten(x +: left, xs, right)
  case (left, SEmpty(), right) => fairFlatten(left ++ right)
  case (left, xs: SDelay[SStream[T]], right) => {
    if(left.isEmpty)
      SDelay(doFairFlatten(right, xs.get, left))
    else {
      val head = left.head
      val rest = left.tail
      head match {
        case SCons(x, headRest) => SCons(x, doFairFlatten(headRest +: rest, xs, right))
        case SEmpty() => doFairFlatten(rest, xs, right)
        case head: SDelay[T] => SDelay(doFairFlatten(rest, xs.get, head.get +: right))
      }
    }
  }
}

implicit object NaiveSearcher extends Searcher {
  // DFSi or DFSbi
  var balenced = true

  def flatten[T](xs: ParVector[SStream[T]]): SStream[T] = if(balenced) fairFlatten(xs) else unfairFlatten(xs)
  def flatten[T](xs: SStream[SStream[T]]): SStream[T] = if(balenced) fairFlatten(xs) else unfairFlatten(xs)

  override def run(state: State, goal: Goal): Stream[State] = exec(state, goal).toStream

  var enableParallel = false
  private val reduceTimes = 4

  private def parallelReduce[T](xs: ParVector[SStream[T]]): ParVector[SStream[T]] =
    if(enableParallel) xs.map(_.reduce(reduceTimes))
    else xs

  private def parallelReduce[T](xs: SStream[SStream[T]]): SStream[SStream[T]] =
    if(enableParallel) xs.reduce(reduceTimes).parMapImmediate(_.reduce(reduceTimes))
    else xs

  def exec(state: State, goal: Goal): SStream[State] =
    goal match {
      case goal: GoalBasic => SStream.from(goal.execute(state))
      case GoalDisj(xs) => SDelay(flatten(parallelReduce(xs.par.map(exec(state, _)))))
      case GoalConj(xs) => if (xs.isEmpty) SStream(state) else {
        val tail = GoalConj(xs.tail)
        SDelay(flatten(exec(state, xs.head).map(exec(_, tail))))
      }
      case GoalReadSubst(f) => exec(state, f(state.eq.subst))
      case goal: GoalDelay => SDelay(exec(state, goal.get))
      case GoalDisjU(xs) =>
        if (xs.isEmpty)
          SStream.empty
        else SDelay {
          val (test, goal) = xs.head
          val rest = xs.tail
          exec(state, test).take1FlatMap({
            exec(state, GoalDisjU(rest))
          }, { state => exec(state, goal) })
        }
      case GoalDisjA(xs) =>
        if (xs.isEmpty)
          SStream.empty
        else SDelay {
          val (test, goal) = xs.head
          val rest = xs.tail
          exec(state, test).caseOnEmpty({
            exec(state, GoalDisjA(rest))
          }, { states => flatten(states.map(exec(_, goal))) })
        }
    }
}