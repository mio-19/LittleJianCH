package littlejian

import scala.collection.parallel.immutable.ParVector

sealed trait Goal

sealed trait GoalBasic extends Goal

final case class GoalEq[T](x: VarOr[T], y: VarOr[T])(implicit unifier: Unifier[T]) extends GoalBasic

final case class GoalNotEq[T](x: VarOr[T], y: VarOr[T])(implicit unifier: Unifier[T]) extends GoalBasic

import scala.reflect.ClassTag

final case class GoalPredType[T](cls: ClassTag[_], x: VarOr[T])(implicit unifier: Unifier[T]) extends GoalBasic

final case class GoalPredNotType[T](cls: ClassTag[_], x: VarOr[T])(implicit unifier: Unifier[T]) extends GoalBasic

sealed trait GoalControl extends Goal

final class GoalDelay(x: => Goal) extends GoalControl {
  lazy val get: Goal = x
}

final case class GoalDisj(xs: ParVector[Goal]) extends GoalControl

final case class GoalConj(xs: ParVector[Goal]) extends GoalControl

object Goal {
  def conde(clauses: ParVector[ParVector[Goal]]): Goal = GoalDisj(clauses.map(GoalConj(_)))
}