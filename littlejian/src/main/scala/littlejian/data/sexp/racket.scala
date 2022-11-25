package littlejian.data.sexp

import littlejian.*
import littlejian.data.*
import littlejian.ext.*

sealed trait RacketAny derives Unify, Inspect

val I$RacketAny = implicitly[Inspect[RacketAny]]

final case class SExpVector(v: Vector[VarOr[SExp]]) extends RacketAny derives Unify, Inspect {
  override def toString: String = s"#(${v.mkString(" ")})"
}

val I$SExpVector = implicitly[Inspect[SExpVector]]

final case class SExpLambda(fn: Seq[VarOr[SExp]] => VarOr[SExp]) extends RacketAny {
  def apply(xs: Seq[VarOr[SExp]]) = fn(xs)
}

implicit val U$SExpLambda: Unify[SExpLambda] = new AtomUnify[SExpLambda]() {}
implicit val I$SExpLambda: Inspect[SExpLambda] = (rec, self, x) => InspectResult(self == x)

implicit def sExpVector(v: Vector[_ <: VarOr[SExp]]): SExpVector = SExpVector(v)

implicit def sExpLambda(fn: ((xs: Seq[VarOr[SExp]]) => VarOr[SExp])): SExpLambda = SExpLambda(fn)

implicit def sExpLambda1(fn: ((a: VarOr[SExp]) => VarOr[SExp])): SExpLambda = sExpLambda({
  case Seq(a) => fn(a)
  case _ => throw new IllegalArgumentException("Not 1 arg")
})
implicit def sExpLambda2(fn: ((a: VarOr[SExp], b: VarOr[SExp]) => VarOr[SExp])): SExpLambda = sExpLambda({
  case Seq(a, b) => fn(a, b)
  case _ => throw new IllegalArgumentException("Not 2 arg")
})
implicit def sExpLambda3(fn: ((a: VarOr[SExp], b: VarOr[SExp], c: VarOr[SExp]) => VarOr[SExp])): SExpLambda = sExpLambda({
  case Seq(a, b, c) => fn(a, b, c)
  case _ => throw new IllegalArgumentException("Not 3 arg")
})
