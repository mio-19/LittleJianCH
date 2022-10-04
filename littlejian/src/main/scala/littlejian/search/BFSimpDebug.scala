package littlejian.search

import littlejian.*

import scala.collection.parallel.immutable.ParVector
import collection.parallel.CollectionConverters.*
import scala.annotation.tailrec

implicit object BFSimpDebug extends Searcher {
  override def run(state: State, goal: Goal): Stream[State] = ???

  def runInternal(state: State, goal: Goal): Option[Vector[State | Request]/*must have at least one state */] = ???

  def run1Internal(state: State, goal: Goal): Option[State] = ???

  final case class Request(state: State, goals: Vector[Goal]) {
    override def toString: String = s"${goals.map(_.toString).mkString(" && ")} on ${state.toString}"
  }

  object Request {
    def apply(base: State | Request, goals: Vector[Goal]):Request  = base match {
      case s: State => new Request(s, goals)
      case r: Request => new Request(r.state, r.goals ++ goals)
    }
    def apply(base: State | Request, goal: Goal): Request = base match {
      case s: State => new Request(s, Vector(goal))
      case r: Request => new Request(r.state, r.goals :+ goal)
    }
    def apply(base: State, goal: Goal) = new Request(base, Vector(goal))
  }

  def exec(candidates: Vector[Request]): Vector[State | Request] = ???

  def exec(state: State, goal: Goal): Vector[State | Request] =
    goal match {
      case goal: GoalBasic => Vector.from(goal.execute(state))
      case GoalDisj(xs) => xs.map(Request(state, _))
      case GoalConj(xs) => if (xs.isEmpty) Vector(state) else {
        val tail = xs.tail
        exec(state, xs.head).map(Request(_, tail))
      }
      case GoalReadSubst(f) => exec(state, f(state.eq.subst))
      case goal: GoalDelay => Vector(Request(state, goal.get))
      case GoalDisjU(xs) =>
        if (xs.isEmpty)
          Vector.empty
        else {
          val (test, goal) = xs.head
          val rest = xs.tail
          run1Internal(state, test) match {
            case Some(state) => Vector(Request(state, goal))
            case None => Vector(Request(state, GoalDisjU(rest)))
          }
        }
      case GoalDisjA(xs) =>
        if (xs.isEmpty)
          Vector.empty
        else {
          val (test, goal) = xs.head
          val rest = xs.tail
          runInternal(state, test) match {
            case Some(states) => states.map(Request(_, goal))
            case None => Vector(Request(state, GoalDisjA(rest)))
          }
        }
    }
}