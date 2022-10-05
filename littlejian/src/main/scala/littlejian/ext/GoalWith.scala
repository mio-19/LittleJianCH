package littlejian.ext

import littlejian.*

import scala.annotation.targetName
import scala.language.implicitConversions

type Rel[T] = GoalWith[VarOr[T]]

object Rel {
  val success: Rel[Unit] = GoalWith(Goal.success, ())
  def apply[T](x: VarOr[T]): Rel[T] = GoalWith(Goal.success, x)
}

private def conj2(x: Goal, y: Goal): Goal =
  if (x eq Goal.success) y
  else if (y eq Goal.success) x
  else GoalConj(x, y)

final case class GoalWith[T](provider: (T => Goal) => Goal) {
  inline def map[U](f: T => U): GoalWith[U] = GoalWith[U](k => provider(t => k(f(t))))

  inline def flatMap[U](f: T => GoalWith[U]): GoalWith[U] = GoalWith[U](k => provider(t => f(t).provider(k)))

  inline def withFilter(p: T => Boolean): GoalWith[T] = GoalWith[T](k => provider(t => if (p(t)) k(t) else Goal.failure))

  inline def goal: Goal = provider(_ => Goal.success)

  inline def >>[U](next: GoalWith[U]): GoalWith[U] = for {
    ignored <- this
    result <- next
  } yield result
}

object GoalWith {
  inline def apply[T](provider: (T => Goal) => Goal): GoalWith[T] = new GoalWith[T](provider)
  inline def apply[T](goal: Goal, value: T): GoalWith[T] = new GoalWith(k => conj2(goal, k(value)))

  inline def apply[T](value: T): GoalWith[T] = new GoalWith(_ (value))

  inline def apply(goal: Goal): GoalWith[Unit] = new GoalWith(k => conj2(goal, k(())))

  @targetName("apply2") def apply[T](fn: VarOr[T] => Goal): GoalWith[VarOr[T]] = new GoalWith(k => callWithFresh[T] { v => conj2(fn(v), k(v)) })
}

@inline implicit def rel2lam[T](x: Rel[T])(implicit unifier: Unifier[T]): VarOr[T] => Goal = arg => x.provider(k => k === arg)

@inline implicit def packGoalWith[T](x: T): GoalWith[T] = GoalWith(x)

inline implicit def lambdaToRel[T](fn: VarOr[T] => Goal): Rel[T] = GoalWith(fn)

inline implicit def goalWithUnitToGoal(goal: GoalWith[Unit]): Goal = goal.goal

inline implicit def toGoalWithUnitToGoal[T](goal: T => GoalWith[Unit]): T => Goal = x => goal(x).goal

inline implicit def goalToRelUnit(goal: Goal): Rel[Unit] = GoalWith(goal, ())
