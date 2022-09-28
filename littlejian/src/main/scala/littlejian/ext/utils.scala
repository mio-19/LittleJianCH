package littlejian.ext

import littlejian._
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

def hole[T]: VarOr[T] = new Var[T]
def fresh[T](block: VarOr[T] => Goal): Goal = GoalDelay(block(hole))
def fresh[T, U](block: VarOr[T] => Rel[U]): Rel[U] = block(hole)

implicit class EqOps[T](x: VarOr[T]) {
  def ===(y: VarOr[T])(implicit unifier: Unifier[T]): Goal = GoalEq(x, y)

  def =/=(y: VarOr[T])(implicit unifier: Unifier[T]): Goal = GoalNotEq(x, y)
}

implicit class TypePredOps[T](x: VarOr[T]) {
  def isType[T](implicit t: ClassTag[T]): Goal = GoalPredType(t, x)

  def isNotType[T](implicit t: ClassTag[T]): Goal = GoalPredNotType(t, x)
}

def begin(xs: => Goal*): Goal = GoalDelay(GoalConj(xs))

def conde(xs: => Goal*): Goal = GoalDelay(GoalDisj(xs))

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
