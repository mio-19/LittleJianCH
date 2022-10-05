package littlejian.search

import littlejian.*

import scala.collection.parallel.immutable.ParVector
import collection.parallel.CollectionConverters.*
import scala.annotation.tailrec

implicit object BFSimpDebug extends Searcher {
  override def run(state: State, goal: Goal): Stream[State] =
    prettyPrintContext.defaultWith(new PrettyPrintContext(Subst.empty)) {
      exec(Vector(Request(state, goal)))
    }

  def runInternal(state: State, goal: Goal): Option[Vector[State | Request] /*must have at least one state */ ] = {
    val next = exec(state, goal)
    if (next.exists(_.isInstanceOf[State])) Some(next)
    else runInternal(next.asInstanceOf[Vector[Request]])
  }

  @tailrec def runInternal(xs: Vector[Request]): Option[Vector[State | Request] /*must have at least one state */ ] = {
    val next = xs.flatMap(_.Exec)
    if (next.exists(_.isInstanceOf[State])) Some(next)
    else runInternal(next.asInstanceOf[Vector[Request]])
  }

  def run1Internal(state: State, goal: Goal): Option[State] = BFSimp.run(state, goal).headOption

  final case class Request(state: State, goals: Vector[Goal]) {
    override def toString: String = {
      val c = state.printConstraints
      if (c.nonEmpty) s"${goals.map(_.toString).mkString(" && ")} on ${state.printConstraints}"
      else goals.map(_.toString).mkString(" && ")
    }

    def goal: Goal = if (goals.size == 1) goals.head else GoalConj(goals)

    def Exec: Vector[State | Request] = exec(state, goal)
  }

  object Request {
    def apply(base: State | Request, goals: Vector[Goal]): Request = base match {
      case s: State => new Request(s, goals)
      case r: Request => new Request(r.state, r.goals ++ goals)
    }

    def apply(base: State | Request, goal: Goal): Request = base match {
      case s: State => new Request(s, Vector(goal))
      case r: Request => new Request(r.state, r.goals :+ goal)
    }

    def apply(base: State, goal: Goal) = new Request(base, Vector(goal))
  }

  private def execNoTail(candidates: Vector[Request]): Stream[State] = exec(candidates)

  @tailrec def exec(candidates: Vector[Request]): Stream[State] = {
    if(candidates.isEmpty) return Stream.empty
    println(s"\n\n---- Running ----:\n${candidates.map(_.toString).distinct.mkString("\n-- Or --\n")}\n\n")
    val next = candidates.flatMap(_.Exec)
    val (result0, rest0) = next.partition(_.isInstanceOf[State])
    val result = result0.asInstanceOf[Vector[State]]
    val rest = rest0.asInstanceOf[Vector[Request]]
    if (result.isEmpty) exec(rest)
    else Stream.from(result) #::: execNoTail(rest)
  }

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