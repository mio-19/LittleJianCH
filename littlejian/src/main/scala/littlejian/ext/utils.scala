package littlejian.ext

import littlejian.*

import scala.annotation.targetName
import scala.collection.parallel.immutable.ParVector
import scala.reflect.ClassTag

implicit class StateResolve(self: State) {
  def resolve[T](x: VarOr[T]): VarOr[T] = self.eq.subst.walk(x)
}

implicit class VarGet[T](self: Var[T]) {
  def get(state: State): VarOr[T] = state.eq.subst.walk(self)
}

implicit class GoalOps(x: => Goal) {
  def &&(y: => Goal): Goal = GoalConj(GoalDelay(x), GoalDelay(y))

  def ||(y: => Goal): Goal = GoalDisj(GoalDelay(x), GoalDelay(y))
}

def fresh[T]: Rel[T] = new Var[T]
@targetName("fresh2") def fresh[T, U]: GoalWith[(VarOr[T], VarOr[U])] = for {
  x <- fresh[T]
  y <- fresh[U]
} yield (x, y)
@targetName("fresh3") def fresh[T, U, V]: GoalWith[(VarOr[T], VarOr[U], VarOr[V])] = for {
  x <- fresh[T]
  y <- fresh[U]
  z <- fresh[V]
} yield (x, y, z)
def fresh[T, U](f: (VarOr[T], VarOr[U]) => Goal): Goal = callWithFresh[T] { t =>
  callWithFresh[U] { u =>
    f(t, u)
  }
}
def fresh[T, U, V](f: (VarOr[T], VarOr[U], VarOr[V]) => Goal): Goal = callWithFresh[T] { t =>
  callWithFresh[U] { u =>
    callWithFresh[V] { v =>
      f(t, u, v)
    }
  }
}


implicit class EqOps[T](x: VarOr[T]) {
  def ===(y: VarOr[T])(implicit unifier: Unifier[T]): Goal = GoalEq(x, y)

  def =/=(y: VarOr[T])(implicit unifier: Unifier[T]): Goal = GoalNotEq(x, y)
}

def compare[T](x: VarOr[T], y: VarOr[T])(equals: => Goal)(notEquals: => Goal)(implicit unifier: Unifier[T]): Goal = conde(
  begin(x === y, equals), begin(x =/= y, notEquals)
)

def compare[T, U](x: VarOr[T], y: VarOr[T])(equals: => Rel[U])(notEquals: => Rel[U])(implicit t: Unifier[T], u: Unifier[U]): Rel[U] = conde(
  begin(x === y, equals), begin(x =/= y, notEquals)
)

implicit class VarOrPredOps[T](x: VarOr[T]) {
  def isType[T](implicit t: ClassTag[T]): Goal = GoalPredType(t, x)

  def isNotType[T](implicit t: ClassTag[T]): Goal = GoalPredNotType(t, x)

  def absent(absent: Any)(implicit inspector: Inspector[T]): Goal = GoalAbsent(WithInspector(x)(inspector), absent)
}

implicit class VarOrCast[T](x: VarOr[T]) {
  def cast[U](implicit u: ClassTag[U], unifier: Unifier[U]): Rel[U] = {
    val result = hole[U]
    begin(
      x.isType[U],
      result === x.asInstanceOf[VarOr[U]],
      result
    )
  }
}

def begin(xs: => Goal*): Goal = GoalDelay(GoalConj(xs))

def conde(xs: => Goal*): Goal = GoalDelay(GoalDisj(xs))

def conda(xs: => (Goal, Goal)*): Goal = GoalDelay(GoalDisjA(xs))
def condu(xs: => (Goal, Goal)*): Goal = GoalDelay(GoalDisjU(xs))

implicit class EqRelOps[T](x: Rel[T]) {
  def ===(y: Rel[T])(implicit unifier: Unifier[T]): Goal = begin(x.goal, y.goal, x.x === y.x)

  def =/=(y: Rel[T])(implicit unifier: Unifier[T]): Goal = begin(x.goal, y.goal, x.x =/= y.x)
}

private def conde_[T](xs: (() => Rel[T])*)(implicit unifier: Unifier[T]): Rel[T] = {
  val v = new Var[T]
  GoalWith(GoalDisj(xs.map(arg => GoalDelay {
    val x = arg()
    GoalConj(x.goal, x.x === v)
  })), v)
}

// print((1 to 10).map(x=>s"def begin[T](${(1 to x).map(i=>s"p${i.toString}: =>GoalWith[_], ").reduce(_+_)}r: GoalWith[T]): GoalWith[T] = GoalWith(begin(${(1 to x).map(i=>s"p${i.toString}.goal, ").reduce(_+_)}r.goal), r.x)\n").reduce(_+_))
// print((1 to 10).map(x=>s"def conde[T](${(1 to x).map(i=>s"p${i.toString}: =>Rel[T]").mkString(", ")})(implicit unifier: Unifier[T]): Rel[T] = conde_(${(1 to x).map(i=>s"() => p${i.toString}").mkString(", ")})\n").mkString(""))
def begin[T](p1: => GoalWith[_], r: GoalWith[T]): GoalWith[T] = GoalWith(begin(p1.goal, r.goal), r.x)
def begin[T](p1: => GoalWith[_], p2: => GoalWith[_], r: GoalWith[T]): GoalWith[T] = GoalWith(begin(p1.goal, p2.goal, r.goal), r.x)
def begin[T](p1: => GoalWith[_], p2: => GoalWith[_], p3: => GoalWith[_], r: GoalWith[T]): GoalWith[T] = GoalWith(begin(p1.goal, p2.goal, p3.goal, r.goal), r.x)
def begin[T](p1: => GoalWith[_], p2: => GoalWith[_], p3: => GoalWith[_], p4: => GoalWith[_], r: GoalWith[T]): GoalWith[T] = GoalWith(begin(p1.goal, p2.goal, p3.goal, p4.goal, r.goal), r.x)
def begin[T](p1: => GoalWith[_], p2: => GoalWith[_], p3: => GoalWith[_], p4: => GoalWith[_], p5: => GoalWith[_], r: GoalWith[T]): GoalWith[T] = GoalWith(begin(p1.goal, p2.goal, p3.goal, p4.goal, p5.goal, r.goal), r.x)
def begin[T](p1: => GoalWith[_], p2: => GoalWith[_], p3: => GoalWith[_], p4: => GoalWith[_], p5: => GoalWith[_], p6: => GoalWith[_], r: GoalWith[T]): GoalWith[T] = GoalWith(begin(p1.goal, p2.goal, p3.goal, p4.goal, p5.goal, p6.goal, r.goal), r.x)
def begin[T](p1: => GoalWith[_], p2: => GoalWith[_], p3: => GoalWith[_], p4: => GoalWith[_], p5: => GoalWith[_], p6: => GoalWith[_], p7: => GoalWith[_], r: GoalWith[T]): GoalWith[T] = GoalWith(begin(p1.goal, p2.goal, p3.goal, p4.goal, p5.goal, p6.goal, p7.goal, r.goal), r.x)
def begin[T](p1: => GoalWith[_], p2: => GoalWith[_], p3: => GoalWith[_], p4: => GoalWith[_], p5: => GoalWith[_], p6: => GoalWith[_], p7: => GoalWith[_], p8: => GoalWith[_], r: GoalWith[T]): GoalWith[T] = GoalWith(begin(p1.goal, p2.goal, p3.goal, p4.goal, p5.goal, p6.goal, p7.goal, p8.goal, r.goal), r.x)
def begin[T](p1: => GoalWith[_], p2: => GoalWith[_], p3: => GoalWith[_], p4: => GoalWith[_], p5: => GoalWith[_], p6: => GoalWith[_], p7: => GoalWith[_], p8: => GoalWith[_], p9: => GoalWith[_], r: GoalWith[T]): GoalWith[T] = GoalWith(begin(p1.goal, p2.goal, p3.goal, p4.goal, p5.goal, p6.goal, p7.goal, p8.goal, p9.goal, r.goal), r.x)
def begin[T](p1: => GoalWith[_], p2: => GoalWith[_], p3: => GoalWith[_], p4: => GoalWith[_], p5: => GoalWith[_], p6: => GoalWith[_], p7: => GoalWith[_], p8: => GoalWith[_], p9: => GoalWith[_], p10: => GoalWith[_], r: GoalWith[T]): GoalWith[T] = GoalWith(begin(p1.goal, p2.goal, p3.goal, p4.goal, p5.goal, p6.goal, p7.goal, p8.goal, p9.goal, p10.goal, r.goal), r.x)

def conde[T](p1: => Rel[T])(implicit unifier: Unifier[T]): Rel[T] = conde_(() => p1)
def conde[T](p1: => Rel[T], p2: => Rel[T])(implicit unifier: Unifier[T]): Rel[T] = conde_(() => p1, () => p2)
def conde[T](p1: => Rel[T], p2: => Rel[T], p3: => Rel[T])(implicit unifier: Unifier[T]): Rel[T] = conde_(() => p1, () => p2, () => p3)
def conde[T](p1: => Rel[T], p2: => Rel[T], p3: => Rel[T], p4: => Rel[T])(implicit unifier: Unifier[T]): Rel[T] = conde_(() => p1, () => p2, () => p3, () => p4)
def conde[T](p1: => Rel[T], p2: => Rel[T], p3: => Rel[T], p4: => Rel[T], p5: => Rel[T])(implicit unifier: Unifier[T]): Rel[T] = conde_(() => p1, () => p2, () => p3, () => p4, () => p5)
def conde[T](p1: => Rel[T], p2: => Rel[T], p3: => Rel[T], p4: => Rel[T], p5: => Rel[T], p6: => Rel[T])(implicit unifier: Unifier[T]): Rel[T] = conde_(() => p1, () => p2, () => p3, () => p4, () => p5, () => p6)
def conde[T](p1: => Rel[T], p2: => Rel[T], p3: => Rel[T], p4: => Rel[T], p5: => Rel[T], p6: => Rel[T], p7: => Rel[T])(implicit unifier: Unifier[T]): Rel[T] = conde_(() => p1, () => p2, () => p3, () => p4, () => p5, () => p6, () => p7)
def conde[T](p1: => Rel[T], p2: => Rel[T], p3: => Rel[T], p4: => Rel[T], p5: => Rel[T], p6: => Rel[T], p7: => Rel[T], p8: => Rel[T])(implicit unifier: Unifier[T]): Rel[T] = conde_(() => p1, () => p2, () => p3, () => p4, () => p5, () => p6, () => p7, () => p8)
def conde[T](p1: => Rel[T], p2: => Rel[T], p3: => Rel[T], p4: => Rel[T], p5: => Rel[T], p6: => Rel[T], p7: => Rel[T], p8: => Rel[T], p9: => Rel[T])(implicit unifier: Unifier[T]): Rel[T] = conde_(() => p1, () => p2, () => p3, () => p4, () => p5, () => p6, () => p7, () => p8, () => p9)
def conde[T](p1: => Rel[T], p2: => Rel[T], p3: => Rel[T], p4: => Rel[T], p5: => Rel[T], p6: => Rel[T], p7: => Rel[T], p8: => Rel[T], p9: => Rel[T], p10: => Rel[T])(implicit unifier: Unifier[T]): Rel[T] = conde_(() => p1, () => p2, () => p3, () => p4, () => p5, () => p6, () => p7, () => p8, () => p9, () => p10)
