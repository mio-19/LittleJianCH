package littlejian.ext

import littlejian._

def begin(xs: Goal*): Goal = GoalConj(xs)

def conde(xs: Goal*): Goal = GoalDisj(xs)

// print((1 to 10).map(x=>s"def begin[T](${(1 to x).map(i=>s"p${i.toString}: GoalWith[_], ").reduce(_+_)}r: GoalWith[T]): GoalWith[T] = GoalWith(begin(${(1 to x).map(i=>s"p${i.toString}.goal, ").reduce(_+_)}r.goal), r.x)\n").reduce(_+_))
// print((1 to 10).map(x=>s"def conde[T](${(1 to x).map(i=>s"p${i.toString}: GoalWith[_], ").reduce(_+_)}r: GoalWith[T]): GoalWith[T] = GoalWith(conde(${(1 to x).map(i=>s"p${i.toString}.goal, ").reduce(_+_)}r.goal), r.x)\n").reduce(_+_))
def begin[T](p1: GoalWith[_], r: GoalWith[T]): GoalWith[T] = GoalWith(begin(p1.goal, r.goal), r.x)
def begin[T](p1: GoalWith[_], p2: GoalWith[_], r: GoalWith[T]): GoalWith[T] = GoalWith(begin(p1.goal, p2.goal, r.goal), r.x)
def begin[T](p1: GoalWith[_], p2: GoalWith[_], p3: GoalWith[_], r: GoalWith[T]): GoalWith[T] = GoalWith(begin(p1.goal, p2.goal, p3.goal, r.goal), r.x)
def begin[T](p1: GoalWith[_], p2: GoalWith[_], p3: GoalWith[_], p4: GoalWith[_], r: GoalWith[T]): GoalWith[T] = GoalWith(begin(p1.goal, p2.goal, p3.goal, p4.goal, r.goal), r.x)
def begin[T](p1: GoalWith[_], p2: GoalWith[_], p3: GoalWith[_], p4: GoalWith[_], p5: GoalWith[_], r: GoalWith[T]): GoalWith[T] = GoalWith(begin(p1.goal, p2.goal, p3.goal, p4.goal, p5.goal, r.goal), r.x)
def begin[T](p1: GoalWith[_], p2: GoalWith[_], p3: GoalWith[_], p4: GoalWith[_], p5: GoalWith[_], p6: GoalWith[_], r: GoalWith[T]): GoalWith[T] = GoalWith(begin(p1.goal, p2.goal, p3.goal, p4.goal, p5.goal, p6.goal, r.goal), r.x)
def begin[T](p1: GoalWith[_], p2: GoalWith[_], p3: GoalWith[_], p4: GoalWith[_], p5: GoalWith[_], p6: GoalWith[_], p7: GoalWith[_], r: GoalWith[T]): GoalWith[T] = GoalWith(begin(p1.goal, p2.goal, p3.goal, p4.goal, p5.goal, p6.goal, p7.goal, r.goal), r.x)
def begin[T](p1: GoalWith[_], p2: GoalWith[_], p3: GoalWith[_], p4: GoalWith[_], p5: GoalWith[_], p6: GoalWith[_], p7: GoalWith[_], p8: GoalWith[_], r: GoalWith[T]): GoalWith[T] = GoalWith(begin(p1.goal, p2.goal, p3.goal, p4.goal, p5.goal, p6.goal, p7.goal, p8.goal, r.goal), r.x)
def begin[T](p1: GoalWith[_], p2: GoalWith[_], p3: GoalWith[_], p4: GoalWith[_], p5: GoalWith[_], p6: GoalWith[_], p7: GoalWith[_], p8: GoalWith[_], p9: GoalWith[_], r: GoalWith[T]): GoalWith[T] = GoalWith(begin(p1.goal, p2.goal, p3.goal, p4.goal, p5.goal, p6.goal, p7.goal, p8.goal, p9.goal, r.goal), r.x)
def begin[T](p1: GoalWith[_], p2: GoalWith[_], p3: GoalWith[_], p4: GoalWith[_], p5: GoalWith[_], p6: GoalWith[_], p7: GoalWith[_], p8: GoalWith[_], p9: GoalWith[_], p10: GoalWith[_], r: GoalWith[T]): GoalWith[T] = GoalWith(begin(p1.goal, p2.goal, p3.goal, p4.goal, p5.goal, p6.goal, p7.goal, p8.goal, p9.goal, p10.goal, r.goal), r.x)

def conde[T](p1: GoalWith[_], r: GoalWith[T]): GoalWith[T] = GoalWith(conde(p1.goal, r.goal), r.x)
def conde[T](p1: GoalWith[_], p2: GoalWith[_], r: GoalWith[T]): GoalWith[T] = GoalWith(conde(p1.goal, p2.goal, r.goal), r.x)
def conde[T](p1: GoalWith[_], p2: GoalWith[_], p3: GoalWith[_], r: GoalWith[T]): GoalWith[T] = GoalWith(conde(p1.goal, p2.goal, p3.goal, r.goal), r.x)
def conde[T](p1: GoalWith[_], p2: GoalWith[_], p3: GoalWith[_], p4: GoalWith[_], r: GoalWith[T]): GoalWith[T] = GoalWith(conde(p1.goal, p2.goal, p3.goal, p4.goal, r.goal), r.x)
def conde[T](p1: GoalWith[_], p2: GoalWith[_], p3: GoalWith[_], p4: GoalWith[_], p5: GoalWith[_], r: GoalWith[T]): GoalWith[T] = GoalWith(conde(p1.goal, p2.goal, p3.goal, p4.goal, p5.goal, r.goal), r.x)
def conde[T](p1: GoalWith[_], p2: GoalWith[_], p3: GoalWith[_], p4: GoalWith[_], p5: GoalWith[_], p6: GoalWith[_], r: GoalWith[T]): GoalWith[T] = GoalWith(conde(p1.goal, p2.goal, p3.goal, p4.goal, p5.goal, p6.goal, r.goal), r.x)
def conde[T](p1: GoalWith[_], p2: GoalWith[_], p3: GoalWith[_], p4: GoalWith[_], p5: GoalWith[_], p6: GoalWith[_], p7: GoalWith[_], r: GoalWith[T]): GoalWith[T] = GoalWith(conde(p1.goal, p2.goal, p3.goal, p4.goal, p5.goal, p6.goal, p7.goal, r.goal), r.x)
def conde[T](p1: GoalWith[_], p2: GoalWith[_], p3: GoalWith[_], p4: GoalWith[_], p5: GoalWith[_], p6: GoalWith[_], p7: GoalWith[_], p8: GoalWith[_], r: GoalWith[T]): GoalWith[T] = GoalWith(conde(p1.goal, p2.goal, p3.goal, p4.goal, p5.goal, p6.goal, p7.goal, p8.goal, r.goal), r.x)
def conde[T](p1: GoalWith[_], p2: GoalWith[_], p3: GoalWith[_], p4: GoalWith[_], p5: GoalWith[_], p6: GoalWith[_], p7: GoalWith[_], p8: GoalWith[_], p9: GoalWith[_], r: GoalWith[T]): GoalWith[T] = GoalWith(conde(p1.goal, p2.goal, p3.goal, p4.goal, p5.goal, p6.goal, p7.goal, p8.goal, p9.goal, r.goal), r.x)
def conde[T](p1: GoalWith[_], p2: GoalWith[_], p3: GoalWith[_], p4: GoalWith[_], p5: GoalWith[_], p6: GoalWith[_], p7: GoalWith[_], p8: GoalWith[_], p9: GoalWith[_], p10: GoalWith[_], r: GoalWith[T]): GoalWith[T] = GoalWith(conde(p1.goal, p2.goal, p3.goal, p4.goal, p5.goal, p6.goal, p7.goal, p8.goal, p9.goal, p10.goal, r.goal), r.x)
