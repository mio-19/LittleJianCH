package littlejian.examples.microkanren

import littlejian._
import littlejian.data._
import littlejian.ext._
import littlejian.unifier._

type MKData = (Unit | String | MKPair) | (MKVar | MKGoal | MKThunk | MKMap) | (MKRec | MKReg)

implicit val U$MKData: Unifier[MKData] = U$Union[Unit, String, MKPair, MKVar, MKGoal, MKThunk, MKMap, MKRec, MKReg]

final case class MKVar(id: VarOr[Nat]) extends Product1[VarOr[Nat]]

implicit val U$MKVar: Unifier[MKVar] = U$Product

final class MKPair(a: VarOr[MKData], b: VarOr[MKData]) extends Pair[MKData, MKData](a, b)

def cons(a: VarOr[MKData], b: VarOr[MKData]): MKPair = new MKPair(a, b)

implicit val U$MKPair: Unifier[MKPair] = implicitly[Unifier[Pair[MKData, MKData]]].asInstanceOf[Unifier[MKPair]]

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

sealed trait MKGoal derives Unifier

final case class MKGoalEq(u: VarOr[MKData], v: VarOr[MKData], env: VarOr[MKMap]) extends MKGoal derives Unifier

final case class MKGoalCallFresh(f: VarOr[MKData], env: VarOr[MKMap]) extends MKGoal derives Unifier

final case class MKGoalConj(g1: VarOr[MKData], g2: VarOr[MKData], env: VarOr[MKMap]) extends MKGoal derives Unifier

final case class MKGoalDisj(g1: VarOr[MKData], g2: VarOr[MKData], env: VarOr[MKMap]) extends MKGoal derives Unifier

final case class MKGoalTop(rand: VarOr[MKData], env: VarOr[MKMap]) extends MKGoal derives Unifier

final case class MKRec(x: VarOr[MKData], exp: VarOr[MKData]) derives Unifier

final case class MKReg(x: VarOr[MKData]) derives Unifier

def list(xs: VarOr[MKData]*): VarOr[MKData] = xs.foldRight[VarOr[MKData]](())(cons)

def microo(x: VarOr[MKData], env: VarOr[MKMap]): Rel[MKData] = ???

def MKMapo(x: VarOr[MKData]): Rel[MKMap] = x.cast[MKMap]
