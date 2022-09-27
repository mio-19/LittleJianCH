package littlejian

sealed trait Goal

sealed trait GoalBasic extends Goal

final case class GoalEq[T](x: VarOr[T], y: VarOr[T])(implicit unifier: Unifier[T]) extends GoalBasic

final case class GoalNotEq[T](x: VarOr[T], y: VarOr[T])(implicit unifier: Unifier[T]) extends GoalBasic

import scala.reflect.ClassTag

final case class GoalPredType[T](cls: ClassTag[_], x: VarOr[T])(implicit unifier: Unifier[T]) extends GoalBasic

final case class GoalPredNotType[T](cls: ClassTag[_], x: VarOr[T])(implicit unifier: Unifier[T]) extends GoalBasic