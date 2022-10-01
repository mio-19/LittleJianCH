package littlejian.ext

import littlejian.*

import java.lang.Package
import scala.language.implicitConversions

type Rel[T] = GoalWith[VarOr[T]]

sealed trait GoalWith[T] {
  // hack for `(a, b) <- val`
  def withFilter(p: T => Boolean): GoalWith[T]
}

private def conjMightOptimize(x: Goal, y: Goal) = if(x == Goal.success) y else GoalConj(x, y)

implicit class GoalWithUnit(self: GoalWith[Unit]) {
  def map(f: Unit => Unit): Goal = self match {
    case GoalWithVal(g, ()) => {
      f(())
      g
    }
    case GoalWithLam(fn) => {
      val g = fn(())
      f(())
      g
    }
  }

  def flatMap[U](f: Unit => GoalWith[U]): GoalWith[U] = {
    val g = self match {
      case GoalWithVal(g, ()) => g
      case GoalWithLam(fn) => fn(())
    }
    f(()) match {
      case GoalWithVal(goal, v) => GoalWithVal(conjMightOptimize(g, goal), v)
      case GoalWithLam(fn) => GoalWithLam { v => conjMightOptimize(g, fn(v)) }
    }
  }

  def map[U](f: Unit => U): GoalWithVal[U] = self match {
    case GoalWithVal(g, ()) => GoalWithVal(g, f(()))
    case GoalWithLam(fn) => GoalWithVal(fn(()), f(()))
  }
}

implicit class GoalAndRel(goal: Goal) {
  def >>[T](rel: GoalWith[T]): GoalWith[T] = rel match {
    case GoalWithVal(g, v) => GoalWithVal(conjMightOptimize(goal, g), v)
    case GoalWithLam(fn) => GoalWithLam { v => conjMightOptimize(goal, fn(v)) }
  }
}

implicit class RelOps[T](self: Rel[T]) {
  def goal: Goal = self match {
    case GoalWithVal(g, _) => g
    case GoalWithLam(fn) => callWithFresh(fn)
  }

  def apply(arg: VarOr[T])(implicit unifier: Unifier[T]): Goal = self match {
    case GoalWithVal(goal, x) => conjMightOptimize(goal, x === arg)
    case GoalWithLam(fn) => fn(arg)
  }

  def map(f: VarOr[T] => Unit): Goal = self match {
    case GoalWithVal(goal, x) => {
      f(x)
      goal
    }
    case GoalWithLam(fn) => callWithFresh[T] { v =>
      val result = fn(v)
      f(v)
      result
    }
  }

  def map[U](f: VarOr[T] => VarOr[U])(implicit tu: Unifier[T], uu: Unifier[U]): Rel[U] = self match {
    case x: GoalWithVal[_] => x.map(f)
    case GoalWithLam(fn) => GoalWithLam[VarOr[U]] { y =>
      callWithFresh[T] { x =>
        conjMightOptimize(fn(x), f(x) === y)
      }
    }
  }

  def flatMap[U](f: VarOr[T] => Rel[U])(implicit tu: Unifier[T], uu: Unifier[U]): Rel[U] = self match {
    case x: GoalWithVal[_] => x.flatMap(f)
    case GoalWithLam(fn) => GoalWithLam[VarOr[U]] { y =>
      callWithFresh[T] { x =>
        conjMightOptimize(fn(x), f(x)(y))
      }
    }
  }
}

final case class GoalWithVal[T](g: Goal, x: T) extends GoalWith[T] {
  def map[U](f: T => U): GoalWithVal[U] = GoalWithVal(g, f(x))

  def flatMap[U](f: T => GoalWith[U]): GoalWith[U] = f(x) match {
    case GoalWithVal(goal2, y) => GoalWithVal(conjMightOptimize(g, goal2), y)
    case GoalWithLam(fn) => GoalWithLam { v => conjMightOptimize(g, fn(v)) }
  }

  override def withFilter(p: T => Boolean): GoalWithVal[T] = {
        if (!p(x)) throw new UnsupportedOperationException("filter not supported")
        this
      }
}

final case class GoalWithLam[T](fn: T => Goal) extends GoalWith[T] {
  def apply(arg: T): Goal = fn(arg)

  override def withFilter(p: T => Boolean): GoalWithLam[T] = GoalWithLam { x =>
    if (!p(x)) throw new UnsupportedOperationException("filter not supported")
    fn(x)
  }
}

implicit def packGoalWith[T](x: T): GoalWithVal[T] = GoalWithVal(Goal.success, x)

implicit def lambdaToRel[T](fn: T => Goal): GoalWithLam[T] = GoalWithLam(fn)

implicit def goalWithUnitToGoal(goal: GoalWith[Unit]): Goal = goal match {
  case GoalWithLam(fn) => fn(())
  case GoalWithVal(g, ()) => g
}

implicit def toGoalWithUnitToGoal[T](goal: T => GoalWith[Unit]): T => Goal = x => goalWithUnitToGoal(goal(x))

implicit def goalToGoalWithUnit(goal: Goal): GoalWith[Unit] = GoalWithVal(goal, ())

implicit def relUnitToGoal(goal: Rel[Unit]): Goal = goal(())