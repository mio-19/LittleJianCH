package littlejian.examples.microkanren

// https://github.com/jasonhemann/micro-in-mini/blob/master/micro-in-mini.rkt

import littlejian._
import littlejian.data._
import littlejian.ext._
import littlejian.unifier._

type MKData = (Unit | String | MKPair) | (MKVar | MKGoal | MKThunk | MKMap) | (MKRec | MKReg)

implicit val U$MKData: Unifier[MKData] = U$Union(U$Union[Unit, String, MKPair], U$Union[MKVar, MKGoal, MKThunk, MKMap], U$Union[MKRec, MKReg])

final case class MKVar(id: VarOr[Nat]) extends Product1[VarOr[Nat]]

implicit val U$MKVar: Unifier[MKVar] = U$Product

final case class MKPair(a: VarOr[MKData], b: VarOr[MKData]) extends Product2[VarOr[MKData], VarOr[MKData]]

implicit val U$MKPair: Unifier[MKPair] = U$Product

sealed trait MKMap
implicit val U$MKMap: Unifier[MKMap] = U$Union[MKMapEmpty.type, MKMapCons].asInstanceOf[Unifier[MKMap]]
implicit val U$VarOr$MKMap: Unifier[VarOr[MKMap]] = U$VarOr(U$MKMap)

case object MKMapEmpty extends MKMap

implicit val U$MKMapEmpty: Unifier[MKMapEmpty.type] = equalUnifier

final case class MKMapCons(key: VarOr[MKData], value: VarOr[MKData], tail: VarOr[MKMap]) extends MKMap with Product3[VarOr[MKData], VarOr[MKData], VarOr[MKMap]]

implicit val U$MKMapCons: Unifier[MKMapCons] = U$Product

sealed trait MKThunkKind

object MKThunkKind {
  case object Top extends MKThunkKind

  case object Bind extends MKThunkKind

  case object MPlus extends MKThunkKind
}

implicit val U$MKThunkKind: Unifier[MKThunkKind] = equalUnifier

final case class MKThunk(kind: VarOr[MKThunkKind], xs: VarOr[List[VarOr[MKData]]]) extends Product2[VarOr[MKThunkKind], VarOr[List[VarOr[MKData]]]]

implicit val U$MKThunk: Unifier[MKThunk] = U$Product(U$VarOr(U$MKThunkKind), U$VarOr(U$List(U$VarOr(U$MKData))))

type MKGoal = (MKGoalEq | MKGoalCallFresh) | (MKGoalConj | MKGoalDisj) | MKGoalTop
implicit val U$MKGoal: Unifier[MKGoal] = U$Union(U$Union[MKGoalEq, MKGoalCallFresh], U$Union[MKGoalConj, MKGoalDisj], U$MKGoalTop)

final case class MKGoalEq(u: VarOr[MKData], v: VarOr[MKData], env: VarOr[MKMap]) extends Product3[VarOr[MKData], VarOr[MKData], VarOr[MKMap]]

implicit val U$MKGoalEq: Unifier[MKGoalEq] = U$Product

final case class MKGoalCallFresh(f: VarOr[MKData], env: VarOr[MKMap]) extends Product2[VarOr[MKData], VarOr[MKMap]]

implicit val U$MKGoalCallFresh: Unifier[MKGoalCallFresh] = U$Product

final case class MKGoalConj(g1: VarOr[MKData], g2: VarOr[MKData], env: VarOr[MKMap]) extends Product3[VarOr[MKData], VarOr[MKData], VarOr[MKMap]]

implicit val U$MKGoalConj: Unifier[MKGoalConj] = U$Product

final case class MKGoalDisj(g1: VarOr[MKData], g2: VarOr[MKData], env: VarOr[MKMap]) extends Product3[VarOr[MKData], VarOr[MKData], VarOr[MKMap]]

implicit val U$MKGoalDisj: Unifier[MKGoalDisj] = U$Product

final case class MKGoalTop(rand: VarOr[MKData], env: VarOr[MKMap]) extends Product2[VarOr[MKData], VarOr[MKMap]]

implicit val U$MKGoalTop: Unifier[MKGoalTop] = U$Product


final case class MKRec(x: VarOr[MKData], exp: VarOr[MKData]) extends Product2[VarOr[MKData], VarOr[MKData]]

implicit val U$MKRec: Unifier[MKRec] = U$Product

final case class MKReg(x: VarOr[MKData]) extends Product1[VarOr[MKData]]

implicit val U$MKReg: Unifier[MKReg] = U$Product

def list(xs: VarOr[MKData]*): VarOr[MKData] = xs.foldRight[VarOr[MKData]](())(MKPair)

def microo(x: VarOr[MKData], env: VarOr[MKMap]): Rel[MKData] = ???

def MKMapo(x: VarOr[MKData]): Rel[MKMap] = for {
  _ <- x.isType[MKMap]
} yield x.asInstanceOf[VarOr[MKMap]]

def applyEnvo(env: VarOr[MKMap], y: VarOr[MKData]): Rel[MKData] = for {
  (key, value, tail) <- env.is(MKMapCons(_, _, _))
  result <- compare(key, y) {
    conde(
      for {
        (x, exp2) <- value.is(MKRec(_, _))
        result <- microo(list("lambda", list(x), exp2), env)
      } yield result,
      for {
        b <- value.is(MKReg(_))
      } yield b
    )
  } {
    applyEnvo(tail, y)
  }
} yield result

def bindo(xs: VarOr[MKData], g: VarOr[MKData]): Rel[MKData] = conde(
  begin(xs === (), ()),
  for {
    _ <- xs.is(MKThunk(_, _))
  } yield MKThunk(MKThunkKind.Bind, List(xs, g)),
  for {
    (a, d) <- xs.is(MKPair(_, _))
    (aa, da) <- a.is(MKPair(_, _))
    xs1 <- runGoalo(g, aa, da)
    xs2 <- bindo(d, g)
    result <- mpluso(xs1, xs2)
  } yield result
)

def mpluso(xs: VarOr[MKData], ys: VarOr[MKData]): Rel[MKData] = ???

def runGoalo(g: VarOr[MKData], s: VarOr[MKData], c: VarOr[MKData]): Rel[MKData] = ???