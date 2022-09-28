package littlejian.ext

import littlejian._

import scala.language.implicitConversions

type Rel[T] = GoalWith[VarOr[T]]

object Rel {
  val success: Rel[Unit] = GoalWith(Goal.success, ())
}

final case class GoalWith[T](goal: Goal, x: T) {
  def map[U](f: T => U): GoalWith[U] = GoalWith(goal, f(x))

  def flatMap[U](f: T => GoalWith[U]): GoalWith[U] = {
    val result = f(x)
    GoalWith(GoalConj(goal, result.goal), result.x)
  }
}

implicit def packGoalWith[T](x: T): GoalWith[T] = GoalWith(Goal.success, x)

implicit def lambdaToRel[T](fn: VarOr[T] => Goal)(implicit unifier: Unifier[T]): Rel[T] = {
  val result = hole
  GoalWith(fn(result), result)
}

implicit def relToLambda[T](rel: Rel[T])(implicit unifier: Unifier[T]): VarOr[T] => Goal = v => GoalConj(rel.goal, rel.x === v)

implicit def relUnitToGoal(rel: Rel[Unit]): Goal = if (rel.x != ()) throw new IllegalStateException("x != ()") else rel.goal

implicit def goalToRelUnit(goal: Goal): Rel[Unit] = GoalWith(goal, ())