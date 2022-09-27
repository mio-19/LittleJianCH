package littlejian.withval

import littlejian._

import scala.language.implicitConversions

type Rel[T] = GoalWith[VarOr[T]]

final case class GoalWith[T](goal: Goal, x: T)

implicit def packGoalWith[T](x: T): GoalWith[T] = GoalWith(Goal.success, x)

implicit def lambdaToRel[T](fn: VarOr[T] => Goal)(implicit unifier: Unifier[T]): Rel[T] = {
  val result = hole
  GoalWith(fn(result), result)
}