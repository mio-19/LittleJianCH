package littlejian

import scala.collection.parallel.immutable.ParVector

sealed trait Goal

sealed trait GoalBasic extends Goal {
  def execute(state: State): Option[State]
}

final case class GoalEq[T](x: VarOr[T], y: VarOr[T])(implicit unifier: Unifier[T]) extends GoalBasic {
  override def execute(state: State): Option[State] = ???
}

final case class GoalNotEq[T](x: VarOr[T], y: VarOr[T])(implicit unifier: Unifier[T]) extends GoalBasic {
  override def execute(state: State): Option[State] = ???
}

import scala.reflect.ClassTag

type PredTypeTag = ClassTag[_]

final case class GoalPredType[T](tag: PredTypeTag, x: VarOr[T])(implicit unifier: Unifier[T]) extends GoalBasic {
  override def execute(state: State): Option[State] = ???
}

final case class GoalPredNotType[T](tag: PredTypeTag, x: VarOr[T])(implicit unifier: Unifier[T]) extends GoalBasic {
  override def execute(state: State): Option[State] = ???
}

sealed trait GoalControl extends Goal

final class GoalDelay(x: => Goal) extends GoalControl {
  lazy val get: Goal = x
}

final case class GoalDisj(xs: ParVector[Goal]) extends GoalControl

final case class GoalConj(xs: ParVector[Goal]) extends GoalControl

object Goal {
  def conde(clauses: ParVector[ParVector[Goal]]): Goal = GoalDisj(clauses.map(GoalConj(_)))

  val success: Goal = GoalConj(ParVector())
  val failure: Goal = GoalDisj(ParVector())
}