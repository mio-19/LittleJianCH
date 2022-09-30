package littlejian

import scala.annotation.targetName
import scala.collection.parallel.immutable.ParVector

sealed trait Goal

sealed trait GoalBasic extends Goal {
  def execute(state: State): Option[State]
}

final case class GoalEq[T](x: VarOr[T], y: VarOr[T])(implicit unifier: Unifier[T]) extends GoalBasic {
  override def execute(state: State): Option[State] = unifier.unify(x, y)(state.eq.subst) match {
    case Some(subst, ()) => state.setEq(EqState(subst))
    case None => None
  }

  override def toString: String = s"${x} === ${y}"
}

final case class GoalNotEq[T](x: VarOr[T], y: VarOr[T])(implicit unifier: Unifier[T]) extends GoalBasic {
  override def execute(state: State): Option[State] =
    for {
      notEq <- NotEqState.create(state.eq, new NotEqRequest(x, y, unifier), state.notEq)
    } yield state.notEqUpdated(notEq)

  override def toString: String = s"${x} =/= ${y}"
}

import scala.reflect.ClassTag

type PredTypeTag = ClassTag[_]

def checkPredTypeTag[T](tag: PredTypeTag, x: Any): Boolean = tag.runtimeClass.isInstance(x)

final case class GoalPredType[T](tag: PredTypeTag, x: VarOr[T]) extends GoalBasic {
  override def execute(state: State): Option[State] = state.eq.subst.walk(x) match {
    case v: Var[_] => Some(state.predTypeMap(_.insert(v, tag)))
    case x => if (checkPredTypeTag(tag, x)) Some(state) else None
  }

  override def toString: String = s"${x}.isType[${tag}]"
}

final case class GoalPredNotType[T](tag: PredTypeTag, x: VarOr[T]) extends GoalBasic {
  override def execute(state: State): Option[State] = state.eq.subst.walk(x) match {
    case v: Var[_] => Some(state.predNotTypeMap(_.insert(v, tag)))
    case x => if (!checkPredTypeTag(tag, x)) Some(state) else None
  }

  override def toString: String = s"${x}.isNotType[${tag}]"
}

final case class GoalAbsent[T](x: WithInspector[T], absent: Any) extends GoalBasic {
  override def execute(state: State): Option[State] = state.absent.insert(state.eq, this).map(state.absentUpdated)

  override def toString: String = s"${x.x}.absent(${absent})"
}

sealed trait GoalControl extends Goal

val goalDelayEvalLevel = new Parameter[Int]
val defaultGoalDelayEvalLevel = 3

final class GoalDelay(x: => Goal) extends GoalControl {
  def get: Goal = x

  override def toString: String = {
    val level = goalDelayEvalLevel.get.getOrElse(defaultGoalDelayEvalLevel)
    if (level <= 0) super.toString else goalDelayEvalLevel.callWith(level - 1) {
      this.get.toString
    }
  }
}

final case class GoalDisj(xs: ParVector[Goal]) extends GoalControl {
  private def flatten: ParVector[Goal] = xs.map({
    case x: GoalDisj => x.flatten
    case v => ParVector(v)
  }).flatten

  override def toString: String = s"conde(${this.flatten.mkString(", ")})"
}

object GoalDisj {
  def apply(xs: ParVector[Goal]) = new GoalDisj(xs)

  def apply(xs: Seq[Goal]) = new GoalDisj(ParVector(xs *))

  @targetName("applyMul") def apply(xs: Goal*) = new GoalDisj(ParVector(xs *))
}

final case class GoalConj(xs: ParVector[Goal]) extends GoalControl {
  private def flatten: ParVector[Goal] = xs.map({
    case x: GoalConj => x.flatten
    case v => ParVector(v)
  }).flatten

  override def toString: String = s"begin(${this.flatten.mkString(", ")})"
}

final case class GoalReadSubst(f: Subst => Goal) extends GoalControl {
  def apply(subst: Subst): Goal = f(subst)
}

object GoalConj {
  def apply(xs: ParVector[Goal]) = new GoalConj(xs)

  def apply(xs: Seq[Goal]) = new GoalConj(ParVector(xs *))

  @targetName("applyMul") def apply(xs: Goal*) = new GoalConj(ParVector(xs *))
}

object Goal {
  val success: Goal = GoalConj(ParVector())
  val failure: Goal = GoalDisj(ParVector())
}