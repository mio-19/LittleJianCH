package littlejian.ext

import littlejian.*

import scala.annotation.targetName
import scala.collection.parallel.immutable.ParVector
import scala.reflect.ClassTag

implicit class VarOrBooleanOps(x: VarOr[Boolean]) {
  def unary_! : Rel[Boolean] = conde(
    begin(x === true, false),
    begin(x === false, true)
  )

  def &&(y: VarOr[Boolean]): Rel[Boolean] = conde(
    begin(x === true, y),
    begin(x === false, false)
  )

  def ||(y: VarOr[Boolean]): Rel[Boolean] = conde(
    begin(x === true, true),
    begin(x === false, y)
  )
  
  def elim[T](whenTrue: Rel[T])(whenFalse: Rel[T])(implicit unifier: Unify[T]): Rel[T] = conde(
    begin(x === true, whenTrue),
    begin(x === false, whenFalse)
  )
}

implicit class StateResolve(self: State) {
  inline def resolve[T](x: VarOr[T]): VarOr[T] = self.eq.subst.walk(x)
}

implicit class VarGet[T](self: Var[T]) {
  inline def get(state: State): VarOr[T] = state.eq.subst.walk(self)
}

implicit class GoalOps(x: => Goal) {
  def &&(y: Goal): Goal = {
    val gx = x
    val gy = y
    if (gx eq Goal.success) return gy
    if (gy eq Goal.success) return gx
    GoalConj(gx, gy)
  }

  def ||(y: Goal): Goal = {
    val gx = x
    val gy = y
    if (gx eq Goal.success) return gx
    if (gy eq Goal.success) return gy
    GoalDisj(gx, gy)
  }
}

implicit class GoalWithUnitOps(x: GoalWith[Unit]) {
  inline def &&(y: Goal): Goal = GoalOps(x).&&(y)

  inline def ||(y: Goal): Goal = GoalOps(x).&&(y)
}

inline def fresh[T]: GoalWith[Var[T]] = GoalWith(k => callWithFresh[T](k))
@targetName("fresh2") def fresh[T, U]: GoalWith[(Var[T], Var[U])] = for {
  x <- fresh[T]
  y <- fresh[U]
} yield (x, y)
@targetName("fresh3") def fresh[T, U, V]: GoalWith[(Var[T], Var[U], Var[V])] = for {
  x <- fresh[T]
  y <- fresh[U]
  z <- fresh[V]
} yield (x, y, z)
@targetName("fresh4") def fresh[T, U, V, W]: GoalWith[(Var[T], Var[U], Var[V], Var[W])] = for {
  x <- fresh[T]
  y <- fresh[U]
  z <- fresh[V]
  w <- fresh[W]
} yield (x, y, z, w)
inline def fresh[T](f: VarOr[T] => Goal): Goal = callWithFresh[T] { t =>
  f(t)
}
inline def fresh[T, U](f: (VarOr[T], VarOr[U]) => Goal): Goal = callWithFresh[T] { t =>
  callWithFresh[U] { u =>
    f(t, u)
  }
}
inline def fresh[T, U, V](f: (VarOr[T], VarOr[U], VarOr[V]) => Goal): Goal = callWithFresh[T] { t =>
  callWithFresh[U] { u =>
    callWithFresh[V] { v =>
      f(t, u, v)
    }
  }
}


implicit class EqOps[T](x: VarOr[T]) {
  inline def ===(y: VarOr[T])(implicit unifier: Unify[T]): Goal = GoalEq(x, y)

  inline def =/=(y: VarOr[T])(implicit unifier: Unify[T]): Goal = GoalNotEq(x, y)

  inline def ===(y: Rel[T])(implicit unifier: Unify[T]): Goal = for {
    y0 <- y
    _ <- x === y0
  } yield ()

  inline def =/=(y: Rel[T])(implicit unifier: Unify[T]): Goal = for {
    y0 <- y
    _ <- x =/= y0
  } yield ()
}

implicit class EqRelOps[T](x: Rel[T]) {
  inline def ===(y: Rel[T])(implicit unifier: Unify[T]): Goal = for {
    x0 <- x
    y0 <- y
    _ <- x0 === y0
  } yield ()
  inline def =/=(y: Rel[T])(implicit unifier: Unify[T]): Goal = for {
    x0 <- x
    y0 <- y
    _ <- x0 =/= y0
  } yield ()
  inline def ===(y: VarOr[T])(implicit unifier: Unify[T]): Goal = for {
    x0 <- x
    _ <- x0 === y
  } yield ()
  inline def =/=(y: VarOr[T])(implicit unifier: Unify[T]): Goal = for {
    x0 <- x
    _ <- x0 =/= y
  } yield ()
}

inline def compare[T](x: VarOr[T], y: VarOr[T])(equals: => Goal)(notEquals: => Goal)(implicit unifier: Unify[T]): Goal = conde(
  begin(x === y, equals), begin(x =/= y, notEquals)
)

inline def compare[T, U](x: VarOr[T], y: VarOr[T])(equals: => Rel[U])(notEquals: => Rel[U])(implicit t: Unify[T], u: Unify[U]): Rel[U] = conde(
  begin(x === y, equals), begin(x =/= y, notEquals)
)

implicit class VarOrPredOps[T](x: VarOr[T]) {
  inline def isType[T](implicit t: ClassTag[T]): Goal = GoalPredType(t, x)

  inline def isNotType[T](implicit t: ClassTag[T]): Goal = GoalPredNotType(t, x)

  inline def absent(absent: Any)(implicit inspector: Inspect[T]): Goal = GoalAbsent(WithInspector(x)(inspector), absent)
}

implicit class VarOrCast[T](x: VarOr[T]) {
  def cast[U](implicit u: ClassTag[U], unifier: Unify[U]): Rel[U] = for {
    result <- fresh[U]
    _ <- x.isType[U]
    _ <- try {
      result === x.asInstanceOf[VarOr[U]]
    } catch {
      case _: ClassCastException => Goal.failure
    }
  } yield result
}

implicit class VarOrForceApply[T](x: VarOr[T]) {
  @deprecated
  def forceApply[U](f: T => U)(implicit unifier: Unify[U]): Rel[U] = for {
    result <- fresh[U]
    _ <- GoalReadSubst { subst =>
      subst.walk(x) match {
        case _: Var[_] => Goal.failure
        case v => result === f(v.asInstanceOf[T])
      }
    }
  } yield result
}

inline def begin(xs: => Goal*): Goal = GoalDelay(GoalConj(xs))

@inline def conde(xs: => Goal*): Goal = GoalDelay(GoalDisj(xs))

inline def conda(xs: => (Goal, Goal)*): Goal = GoalDelay(GoalDisjA(xs))
inline def condu(xs: => (Goal, Goal)*): Goal = GoalDelay(GoalDisjU(xs))

@inline def conde[T](xs: => Rel[T]*)(implicit unifier: Unify[T]): Rel[T] = {
  (result: VarOr[T]) => GoalDelay(GoalDisj(xs.map(_ (result))))
}

// print((1 to 10).map(x=>s"inline def begin[T](${(1 to x).map(i=>s"p${i.toString}: =>GoalWith[_], ").reduce(_+_)}r: GoalWith[T]): GoalWith[T] = begin(${(1 to x).map(i=>s"p${i.toString}.goal, ").reduce(_+_)}r.goal) >> r\n").reduce(_+_))
inline def begin[T](p1: => GoalWith[_], r: GoalWith[T]): GoalWith[T] = begin(p1.goal, r.goal) >> r
inline def begin[T](p1: => GoalWith[_], p2: => GoalWith[_], r: GoalWith[T]): GoalWith[T] = begin(p1.goal, p2.goal, r.goal) >> r
inline def begin[T](p1: => GoalWith[_], p2: => GoalWith[_], p3: => GoalWith[_], r: GoalWith[T]): GoalWith[T] = begin(p1.goal, p2.goal, p3.goal, r.goal) >> r
inline def begin[T](p1: => GoalWith[_], p2: => GoalWith[_], p3: => GoalWith[_], p4: => GoalWith[_], r: GoalWith[T]): GoalWith[T] = begin(p1.goal, p2.goal, p3.goal, p4.goal, r.goal) >> r
inline def begin[T](p1: => GoalWith[_], p2: => GoalWith[_], p3: => GoalWith[_], p4: => GoalWith[_], p5: => GoalWith[_], r: GoalWith[T]): GoalWith[T] = begin(p1.goal, p2.goal, p3.goal, p4.goal, p5.goal, r.goal) >> r
inline def begin[T](p1: => GoalWith[_], p2: => GoalWith[_], p3: => GoalWith[_], p4: => GoalWith[_], p5: => GoalWith[_], p6: => GoalWith[_], r: GoalWith[T]): GoalWith[T] = begin(p1.goal, p2.goal, p3.goal, p4.goal, p5.goal, p6.goal, r.goal) >> r
inline def begin[T](p1: => GoalWith[_], p2: => GoalWith[_], p3: => GoalWith[_], p4: => GoalWith[_], p5: => GoalWith[_], p6: => GoalWith[_], p7: => GoalWith[_], r: GoalWith[T]): GoalWith[T] = begin(p1.goal, p2.goal, p3.goal, p4.goal, p5.goal, p6.goal, p7.goal, r.goal) >> r
inline def begin[T](p1: => GoalWith[_], p2: => GoalWith[_], p3: => GoalWith[_], p4: => GoalWith[_], p5: => GoalWith[_], p6: => GoalWith[_], p7: => GoalWith[_], p8: => GoalWith[_], r: GoalWith[T]): GoalWith[T] = begin(p1.goal, p2.goal, p3.goal, p4.goal, p5.goal, p6.goal, p7.goal, p8.goal, r.goal) >> r
inline def begin[T](p1: => GoalWith[_], p2: => GoalWith[_], p3: => GoalWith[_], p4: => GoalWith[_], p5: => GoalWith[_], p6: => GoalWith[_], p7: => GoalWith[_], p8: => GoalWith[_], p9: => GoalWith[_], r: GoalWith[T]): GoalWith[T] = begin(p1.goal, p2.goal, p3.goal, p4.goal, p5.goal, p6.goal, p7.goal, p8.goal, p9.goal, r.goal) >> r
inline def begin[T](p1: => GoalWith[_], p2: => GoalWith[_], p3: => GoalWith[_], p4: => GoalWith[_], p5: => GoalWith[_], p6: => GoalWith[_], p7: => GoalWith[_], p8: => GoalWith[_], p9: => GoalWith[_], p10: => GoalWith[_], r: GoalWith[T]): GoalWith[T] = begin(p1.goal, p2.goal, p3.goal, p4.goal, p5.goal, p6.goal, p7.goal, p8.goal, p9.goal, p10.goal, r.goal) >> r
