package littlejian.ext

import littlejian.*

import scala.annotation.targetName
import scala.language.implicitConversions

type Rel[T] = GoalWith[VarOr[T]]

object Rel {
  val success: Rel[Unit] = GoalWith(Goal.success, ())
}

private def conj2(x: Goal, y: Goal): Goal =
  if (x == Goal.success) y
  else if (y == Goal.success) x
  else GoalConj(x, y)

final case class GoalWith[T](provider: (T => Goal) => Goal) {
  def map[U](f: T => U): GoalWith[U] = GoalWith[U](k => provider(t => k(f(t))))

  def flatMap[U](f: T => GoalWith[U]): GoalWith[U] = GoalWith[U](k => provider(t => f(t).provider(k)))

  def withFilter(p: T => Boolean): GoalWith[T] = GoalWith[T](k => provider(t => if (p(t)) k(t) else Goal.failure))

  def goal: Goal = provider(_ => Goal.success)
}

implicit class GoalAppendGoalWith(goal: Goal) {
  def >>[T](x: GoalWith[T]): GoalWith[T] = GoalWith[T](k => conj2(goal, x.provider(k)))
}

object GoalWith {
  def apply[T](provider: (T => Goal) => Goal): GoalWith[T] = new GoalWith[T](provider)
  def apply[T](goal: Goal, value: T): GoalWith[T] = new GoalWith(k => conj2(goal, k(value)))

  def apply[T](value: T): GoalWith[T] = new GoalWith(_ (value))

  def apply(goal: Goal): GoalWith[Unit] = new GoalWith(k => conj2(goal, k(())))

  @targetName("apply2") def apply[T](fn: VarOr[T] => Goal): GoalWith[VarOr[T]] = new GoalWith(k => callWithFresh[T] { v => conj2(fn(v), k(v)) })
}

implicit def rel2lam[T](x: Rel[T])(implicit unifier: Unifier[T]): VarOr[T] => Goal = arg => x.provider(k => k === arg)

implicit def packGoalWith[T](x: T): GoalWith[T] = GoalWith(x)

implicit def lambdaToRel[T](fn: VarOr[T] => Goal): Rel[T] = GoalWith(fn)

implicit def goalWithUnitToGoal(goal: GoalWith[Unit]): Goal = goal.goal

implicit def toGoalWithUnitToGoal[T](goal: T => GoalWith[Unit]): T => Goal = x => goal(x).goal

implicit def goalToRelUnit(goal: Goal): Rel[Unit] = GoalWith(goal, ())