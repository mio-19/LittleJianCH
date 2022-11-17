package littlejian.search

import littlejian.*

import scala.annotation.tailrec

// adapted from https://github.com/cicada-lang/whereabouts/blob/master/src/lang/solver/Task.ts
implicit object XyhDepthFirst extends Xyh(true)

implicit object XyhBreadthFirst extends Xyh(false)

private class Xyh(depthFirst: Boolean) extends Searcher {
  def pursue(state: State, goal: Goal): Vector[Task] = goal match {
    case goal: GoalBasic => goal.execute(state).toVector.map(Task(_))
    case GoalDisj(xs) => xs.map(g => Task(state, Vector(goal, g)))
    case GoalConj(xs) => if (xs.isEmpty) Vector(Task(state)) else {
      val g = xs.head
      val gs = xs.tail
      pursue(state, g).map(t => Task(t.state, t.goals ++ gs))
    }
    case GoalReadSubst(g) => pursue(state, g(state.eq.subst))
    case goal: GoalDelay => Vector(Task(state, Vector(goal.get)))
    case GoalDisjU(gs) => ???
    case GoalDisjA(gs) => ???
  }

  final case class Task(state: State, goals: Vector[Goal]) {
    def step: Option[Vector[Task]] = if (goals.isEmpty)
      None
    else Some {
      val g = goals.head
      val gs = goals.tail
      pursue(state, g).map(task => {
        if (depthFirst) Task(task.state, task.goals ++ gs)
        else Task(task.state, gs ++ task.goals)
      })
    }

    def done = goals.isEmpty
  }

  object Task {
    def apply(x: State): Task = new Task(x, Vector.empty)
  }

  @tailrec private def exec(xs: Vector[Task]): Option[(Vector /*non-empty*/ [State], Vector[Task])] =
    if (xs.isEmpty) None
    else {
      val (dones, rest) = xs.partition(_.done)
      if (dones.nonEmpty) return Some((dones.map(_.state), rest))
      exec(xs.flatMap(_.step.get))
    }

  def exec1(xs: Vector[Task]): SizedStream[State] = exec(xs) match {
    case None => SizedStream.empty
    case Some((dones, rest)) => SizedStream.build(dones, {
      exec1(rest)
    })
  }

  def exec2(state: State, goal: Goal): SizedStream[State] = exec1(Vector(Task(state, Vector(goal))))

  override def run(state: State, goal: Goal): Stream[State] = exec2(state, goal).toStream
}
