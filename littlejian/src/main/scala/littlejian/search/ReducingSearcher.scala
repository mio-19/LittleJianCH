package littlejian.search

import scala.collection.parallel.immutable.ParVector
import littlejian.*
import littlejian.search.GradualSearcher.World

// also somehow broken
implicit object ReducingSearcher extends Searcher {
  // TODO: parallel execution
  private def runBasics(state: State, xs: Seq[GoalBasic]): Option[State] = if (xs.isEmpty) Some(state) else xs.head.execute(state).flatMap(runBasics(_, xs.tail))

  override def run(state: State, goal: Goal): Stream[State] = StateWithGoals(state, Vector(goal)).exec.toStream

  final case class StateWithGoals(state: State, goals: Vector[Goal]) {
    def reduceNotSplit: Option[(StateWithGoals, Vector[GoalDisj])] =
      if (goals.isEmpty) Some((this, Vector.empty))
      else {
        val basics = Seq.newBuilder[GoalBasic]
        val conjs = Seq.newBuilder[GoalConj]
        val disjs = Vector.newBuilder[GoalDisj]
        val readSubsts = Seq.newBuilder[GoalReadSubst]
        val delays = Seq.newBuilder[GoalDelay]
        goals.foreach {
          case goal: GoalBasic => basics += goal
          case goal: GoalConj => conjs += goal
          case goal: GoalDisj => disjs += goal
          case goal: GoalReadSubst => readSubsts += goal
          case goal: GoalDelay => delays += goal
          case goal: GoalControlImpure => throw new UnsupportedOperationException("not implemented")
        }
        runBasics(state, basics.result()) match {
          case None => None
          case Some(state) => {
            val goals = Vector.newBuilder[Goal]
            goals ++= conjs.result().flatMap(_.xs)
            goals ++= readSubsts.result().map(_ (state.eq.subst))
            goals ++= delays.result().map(_.get)
            Some((StateWithGoals(state, goals.result()), disjs.result()))
          }
        }
      }

    def split(xs: Seq[GoalDisj]): ParVector[StateWithGoals] =
      if (xs.isEmpty) ParVector(this)
      else {
        val (head, tail) = (xs.head, xs.tail)
        head.xs.map({ disj =>
          StateWithGoals(state, disj +: goals)
        }).flatMap(_.split(tail))
      }

    def run: (Option[State], ParVector[StateWithGoals]) =
      if (goals.isEmpty) (Some(state), ParVector.empty)
      else this.reduceNotSplit match {
        case None => (None, ParVector.empty)
        case Some((state, disjs)) => (None, state.split(disjs))
      }

    def exec: SStream[State] = this.run match {
      case (Some(state), rest) => SCons(state, SDelay(fairFlatten(rest.map(_.exec))))
      case (None, rest) => SDelay(fairFlatten(rest.map(_.exec)))
    }
  }
}
