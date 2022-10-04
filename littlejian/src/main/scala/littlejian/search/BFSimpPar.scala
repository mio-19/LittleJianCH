package littlejian.search

import littlejian._
import collection.parallel.CollectionConverters._
import scala.collection.parallel.immutable.ParVector

implicit object BFSimpPar extends Searcher {
  override def run(state: State, goal: Goal): Stream[State] = exec(state, goal, initDisjReduceLevel).toStream

  val typicalConjUnit = 10
  val typicalDisjUnit = 10
  val goalReduceLevel: Int = 5
  val initDisjReduceLevel: Int = Runtime.getRuntime.availableProcessors()

  def collectGoalConj(x: GoalConj): Vector[Goal] = {
    if(x.xs.length > typicalConjUnit) return x.xs
    x.xs.flatMap {
      case x: GoalConj => collectGoalConj(x)
      case x: GoalDelay => x.get match {
        case x: GoalConj => collectGoalConj(x)
        case x => Some(x)
      }
      case x => Some(x)
    }
  }

  def collectGoalDisj(x: GoalDisj): Vector[Goal] = {
    if(x.xs.length > typicalDisjUnit) return x.xs
    x.xs.flatMap {
      case x: GoalDisj => collectGoalDisj(x)
      case x: GoalDelay => x.get match {
        case x: GoalDisj => collectGoalDisj(x)
        case x => Some(x)
      }
      case x => Some(x)
    }
  }

  // TODO: par
  def runBasic(state: State, xs: Vector[GoalBasic]): Option[State] = if (xs.isEmpty) Some(state) else xs.head.execute(state).flatMap(runBasic(_, xs.tail))


  def exec(state: State, goal: Goal, disjReduceLevel: Int): SizedStream[State] =
    goal match {
      case goal: GoalBasic => SizedStream.from(goal.execute(state))
      case goal: GoalDisj => {
        val xs = collectGoalDisj(goal)
        val nextDisjReduceLevel = disjReduceLevel / 2
        SizedStream(flatten(xs.par.map(x=>exec(state, x, nextDisjReduceLevel).forceN(disjReduceLevel))))
      }
      case goal: GoalConj => {
        val xs = collectGoalConj(goal)
        if (xs.isEmpty) SizedStream(state) else {
          val (basics, rest) = xs.partition(_.isInstanceOf[GoalBasic])
          runBasic(state, basics.asInstanceOf[Vector[GoalBasic]]) match {
            case Some(state) => if(rest.isEmpty) SizedStream(state) else {
              val tail = GoalConj(rest.tail)
              SizedStream(exec(state, rest.head, disjReduceLevel).appendMapFair(exec(_, tail, disjReduceLevel)))
            }
            case None => SizedStream.empty
          }
        }
      }
      case GoalReadSubst(f) => exec(state, f(state.eq.subst), disjReduceLevel)
      case goal: GoalDelay => SizedStream(exec(state, goal.forceN(goalReduceLevel), disjReduceLevel))
      case GoalDisjU(xs) =>
        if (xs.isEmpty)
          SizedStream.empty
        else SizedStream {
          val (test, goal) = xs.head
          val rest = xs.tail
          exec(state, test, disjReduceLevel).take1FlatMap({
            exec(state, GoalDisjU(rest), disjReduceLevel)
          }, { state => exec(state, goal, disjReduceLevel) })
        }
      case GoalDisjA(xs) =>
        if (xs.isEmpty)
          SizedStream.empty
        else SizedStream {
          val (test, goal) = xs.head
          val rest = xs.tail
          exec(state, test, disjReduceLevel).caseOnEmpty({
            exec(state, GoalDisjA(rest), disjReduceLevel)
          }, { states => states.appendMapFair(exec(_, goal, disjReduceLevel)) })
        }
    }
}
