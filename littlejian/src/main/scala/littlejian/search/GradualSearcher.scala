package littlejian.search

import scala.collection.parallel.immutable.ParVector
import littlejian._
import scala.util.control.Breaks._

implicit object GradualSearcher extends Searcher {
  // TODO: parallel execution
  private def runBasic(state: State, xs: ParVector[GoalBasic]): Option[State] = if (xs.isEmpty) Some(state) else xs.head.execute(state).flatMap(runBasic(_, xs.tail))

  private val reduceLevel = 9

  private def getGoals(originGoals: ParVector[Goal]): ParVector[Goal] = {
    var result = originGoals
    breakable {
      for (_ <- 0 until reduceLevel) {
        val (conjs, rest) = result.partition(x => x.isInstanceOf[GoalConj])
        if (conjs.nonEmpty) result = (rest +: conjs.asInstanceOf[ParVector[GoalConj]].map(_.xs)).flatten
        val (delays, rest1) = result.partition(x => x.isInstanceOf[GoalDelay])
        if (delays.nonEmpty) result = delays.asInstanceOf[ParVector[GoalDelay]].map(_.get) ++ rest1
        if (conjs.isEmpty && delays.isEmpty) break
      }
    }
    result
  }

  private def expandDisj(disjs: ParVector[GoalDisj], world: World): ParVector[World] =
    if (disjs.isEmpty) ParVector(world)
    else {
      val disj = disjs.head
      val rest = disjs.tail
      disj.xs.map(g => expandDisj(rest, World(world.state, g +: world.goals))).flatten
    }

  final case class World(state: State, goals: ParVector[Goal]) {
    def run: (Option[State], ParVector[World]) =
      if (goals.isEmpty) (Some(state), ParVector.empty)
      else (None, {
        val (basics, rest) = getGoals(goals).partition(_.isInstanceOf[GoalBasic])
        runBasic(state, basics.asInstanceOf[ParVector[GoalBasic]]) match {
          case None => ParVector.empty
          case Some(state) => {
            val (disjs, rest1) = rest.partition(x => x.isInstanceOf[GoalDisj])
            expandDisj(disjs.asInstanceOf[ParVector[GoalDisj]], World(state, rest1))
          }
        }
      })

    def exec: SStream[State] = {
      val (ok, rest) = this.run
      ok match {
        case Some(ok) => SCons(ok, flatten(rest.map(_.exec)))
        case None => flatten(rest.map(_.exec))
      }
    }
  }

  override def run(state: State, goal: Goal): Stream[State] = World(state, ParVector(goal)).exec.toStream
}
