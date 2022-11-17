package littlejian.search

import littlejian.*

import scala.collection.parallel.immutable.ParVector
import collection.parallel.CollectionConverters.*
import scala.annotation.tailrec

final class SizedStream[T](val bucket: Vector[T], val thunk: Option[() => SizedStream[T]]) {
  def toStream: Stream[T] = Stream.from(bucket) #::: thunk.map(_ ().toStream).getOrElse(Stream.empty)

  def force1: SizedStream[T] = {
    val rest = thunk match {
      case Some(thunk) => thunk()
      case None => SizedStream.empty[T]
    }
    new SizedStream(bucket ++ rest.bucket, rest.thunk)
  }

  @tailrec def forceN(n: Int): SizedStream[T] = {
    if (n <= 0) this
    else force1.forceN(n - 1)
  }

  def appendFair(other: SizedStream[T]): SizedStream[T] = {
    new SizedStream(bucket ++ other.bucket, (thunk, other.thunk) match {
      case (None, xs) => xs
      case (xs, None) => xs
      case (Some(xs), Some(ys)) => Some(() => xs().appendFair(ys()))
    })
  }

  def appendMapFair[U](f: T => SizedStream[U]): SizedStream[U] = bucket.foldRight(thunk match {
    case None => SizedStream.empty
    case Some(xs) => SizedStream(xs().appendMapFair(f))
  })((x, xs) => f(x).appendFair(xs))

  def caseOnEmpty[U](default: => SizedStream[U], kf: SizedStream[T] => SizedStream[U]): SizedStream[U] = {
    if (bucket.isEmpty) {
      thunk match {
        case None => default
        case Some(xs) => SizedStream(xs().caseOnEmpty(default, kf))
      }
    } else {
      kf(this)
    }
  }

  def take1FlatMap[U](default: => SizedStream[U], f: T => SizedStream[U]): SizedStream[U] = this.caseOnEmpty(default, _.appendMapFair(f))
}

object SizedStream {
  def empty[T]: SizedStream[T] = new SizedStream(Vector.empty, None)

  def build[T](xs: Vector[T], thunk: => SizedStream[T]): SizedStream[T] = new SizedStream[T](xs, Some(() => thunk))

  def apply[T](xs: T*): SizedStream[T] = new SizedStream[T](xs.toVector, None)

  def apply[T](thunk: => SizedStream[T]): SizedStream[T] = new SizedStream[T](Vector.empty, Some(() => thunk))

  def from[T](x: IterableOnce[T]) = new SizedStream[T](Vector.from(x), None)
}

def flatten[T](x: ParVector[SizedStream[T]]): SizedStream[T] =
  if (x.isEmpty) SizedStream.empty
  else x.head.appendFair(flatten(x.tail))

def flatten[T](x: IterableOnce[SizedStream[T]]): SizedStream[T] = flatten(Vector.from(x).par) // TODO: optimize this

implicit object BFSimp extends Searcher {
  override def run(state: State, goal: Goal): Stream[State] = exec(state, goal).toStream

  def exec(state: State, goal: Goal): SizedStream[State] =
    goal match {
      case goal: GoalBasic => SizedStream.from(goal.execute(state))
      case GoalDisj(xs) => SizedStream(flatten(xs.par.map(exec(state, _))))
      case GoalConj(xs) => if (xs.isEmpty) SizedStream(state) else {
        val tail = GoalConj(xs.tail)
        SizedStream(exec(state, xs.head).appendMapFair(exec(_, tail)))
      }
      case GoalReadSubst(f) => exec(state, f(state.eq.subst))
      case goal: GoalDelay => SizedStream(exec(state, goal.get))
      case GoalDisjU(xs) =>
        if (xs.isEmpty)
          SizedStream.empty
        else SizedStream {
          val (test, goal) = xs.head
          val rest = xs.tail
          exec(state, test).take1FlatMap({
            exec(state, GoalDisjU(rest))
          }, { state => exec(state, goal) })
        }
      case GoalDisjA(xs) =>
        if (xs.isEmpty)
          SizedStream.empty
        else SizedStream {
          val (test, goal) = xs.head
          val rest = xs.tail
          exec(state, test).caseOnEmpty({
            exec(state, GoalDisjA(rest))
          }, { states => states.appendMapFair(exec(_, goal)) })
        }
    }
}
