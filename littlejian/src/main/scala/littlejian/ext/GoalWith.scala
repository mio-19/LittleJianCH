package littlejian.ext

import littlejian._

import scala.language.implicitConversions

type Rel[T] = GoalWith[VarOr[T]]

final case class GoalWith[T](goal: Goal, x: T)

implicit def packGoalWith[T](x: T): GoalWith[T] = GoalWith(Goal.success, x)

implicit def lambdaToRel[T](fn: VarOr[T] => Goal)(implicit unifier: Unifier[T]): Rel[T] = {
  val result = hole
  GoalWith(fn(result), result)
}

implicit def relUnitToGoal(rel: Rel[Unit]): Goal = if(rel.x != ()) throw new IllegalStateException("x != ()") else rel.goal

implicit def goalToRelUnit(goal: Goal): Rel[Unit] = GoalWith(goal, ())