package littlejian.search.deprecated

import littlejian.search.*
import littlejian.*

import scala.collection.parallel.immutable.ParVector
import collection.parallel.CollectionConverters._
import scala.util.control.Breaks.{break, breakable}

// Broken, GoalControlImpure not supported
implicit object GradualSearcher extends Searcher {
  // TODO: parallel execution
  private def runBasic(state: State, xs: Vector[GoalBasic]): IterableOnce[State] = if (xs.isEmpty) Some(state) else xs.head.execute(state).flatMap(runBasic(_, xs.tail))

  private val reduceLevel = 3

  private def getGoals(originGoals: Vector[Goal]): Vector[Goal] = {
    var result = originGoals
    breakable {
      for (_ <- 0 until reduceLevel) {
        val (conjs, rest) = result.partition(x => x.isInstanceOf[GoalConj])
        if (conjs.nonEmpty) result = (rest +: conjs.asInstanceOf[Vector[GoalConj]].map(_.xs)).flatten
        val (delays, rest1) = result.partition(x => x.isInstanceOf[GoalDelay])
        if (delays.nonEmpty) result = delays.asInstanceOf[Vector[GoalDelay]].map(_.get) ++ rest1
        if (conjs.isEmpty && delays.isEmpty) break
      }
    }
    result
  }

  private def expandDisj(disjs: Vector[GoalDisj], world: World): Vector[World] =
    if (disjs.isEmpty) Vector(world)
    else {
      val disj = disjs.head
      val rest = disjs.tail
      disj.xs.map(g => expandDisj(rest, World(world.state, g +: world.goals))).flatten
    }

  val taskLimits = 16

  private def runTask(xs: Vector[World]): SStream[State] = {
    if (xs.length > taskLimits) {
      val (a, b) = xs.splitAt(xs.length / 2)
      mplus(runTask(a), SDelay {
        runTask(b)
      })
    }
    val result = xs.map(_.run)
    val states = result.flatMap(_._1)
    val next = SDelay {
      unfairFlatten(result.map(_._2).par.map(runTask))
    }
    SStream.append(states, next)
  }

  final case class World(state: State, goals: Vector[Goal]) {
    def run: (Option[State], Vector[World]) =
      if (goals.isEmpty) (Some(state), Vector.empty)
      else (None, {
        if (goals.exists(_.isInstanceOf[GoalControlImpure])) throw new UnsupportedOperationException("GoalControlImpure not supported")
        val (basics, rest) = getGoals(goals).partition(_.isInstanceOf[GoalBasic])
        Vector.from(runBasic(state, basics.asInstanceOf[Vector[GoalBasic]])).flatMap({state => {
            val (reads, rest0) = rest.partition(x => x.isInstanceOf[GoalReadSubst])
            val goals = reads.asInstanceOf[Vector[GoalReadSubst]].map(_ (state.eq.subst)) ++ rest0
            val (disjs, rest1) = goals.partition(x => x.isInstanceOf[GoalDisj])
            expandDisj(disjs.asInstanceOf[Vector[GoalDisj]], World(state, rest1))
          }
        })
      })

    def exec: SStream[State] = runTask(Vector(this))
  }

  override def run(state: State, goal: Goal): Stream[State] = World(state, Vector(goal)).exec.toStream
}
