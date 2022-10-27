package littlejian.search.deprecated

import littlejian.search.*
import littlejian.*
import collection.parallel.CollectionConverters._

import scala.collection.parallel.immutable.ParVector

// also broken
implicit object ReducingSearcher extends Searcher {
  // TODO: parallel execution
  private def runBasics(state: State, xs: Seq[GoalBasic]): IterableOnce[State] = if (xs.isEmpty) Some(state) else xs.head.execute(state).flatMap(runBasics(_, xs.tail))

  def runSingle(state: State, goal: Goal): Stream[State] = StateWithGoals(state, Vector(goal)).exec.toStream

  def runPar(state: State, goal: Goal): Stream[State] = exec(ParVector(StateWithGoals(state, Vector(goal)))).toStream

  override def run(state: State, goal: Goal): Stream[State] = runSingle(state, goal)

  private val maxTasks = 8

  def exec(xs: ParVector[StateWithGoals]): SStream[State] =
    if (xs.isEmpty) SStream.empty
    else {
      if (xs.length > maxTasks) {
        val (left, right) = xs.splitAt(xs.length / 2)
        mplus(exec(left), exec(right))
      }
      val result = xs.map(_.run)
      val (ok, rest) = (result.flatMap(_._1), result.flatMap(_._2))
      SStream.append(ok, SDelay(exec(rest)))
    }

  final case class StateWithGoals(state: State, goals: Vector[Goal]) {
    def reduceNotSplit: (Vector[StateWithGoals], Vector[GoalDisj]) =
      if (goals.isEmpty) (Vector(this), Vector.empty)
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
        {
          val goals = Vector.newBuilder[Goal]
          goals ++= conjs.result().flatMap(_.xs)
          goals ++= readSubsts.result().map(_ (state.eq.subst))
          goals ++= delays.result().map(_.get)
          val result = goals.result()
          (Vector.from(runBasics(state, basics.result())).map(state => StateWithGoals(state, result)), disjs.result())
        }
      }

    def split1(xs: Seq[GoalDisj]): Vector[StateWithGoals] =
      if (xs.isEmpty) Vector(this)
      else {
        val gs = goals ++ xs.tail
        xs.head.xs.map(g => StateWithGoals(state, g +: gs))
      }

    def run: (Option[State], Vector[StateWithGoals]) =
      if (goals.isEmpty) (Some(state), Vector.empty)
      else this.reduceNotSplit match {
        case (states, disjs) => (None, states.flatMap(_.split1(disjs)))
      }

    def exec: SStream[State] = this.run match {
      case (Some(state), rest) => SCons(state, SDelay(fairFlatten(rest.par.map(_.exec))))
      case (None, rest) => SDelay(fairFlatten(rest.map(_.exec)))
    }
  }
}
