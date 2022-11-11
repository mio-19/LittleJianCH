package littlejian.examples.microkanren

import littlejian._
import littlejian.data._
import littlejian.ext._

type MKData = (Unit | String | MKPair) | (MKVar | MKGoal | MKThunk | MKMap) | (MKRec | MKReg)

implicit val U$MKData: Unify[MKData] = U$Union[Unit, String, MKPair, MKVar, MKGoal, MKThunk, MKMap, MKRec, MKReg]

final case class MKVar(id: VarOr[Nat]) extends Product1[VarOr[Nat]]

implicit val U$MKVar: Unify[MKVar] = U$Product

final class MKPair(a: VarOr[MKData], b: VarOr[MKData]) extends Pair[MKData, MKData](a, b)

def cons(a: VarOr[MKData], b: VarOr[MKData]): MKPair = new MKPair(a, b)

implicit val U$MKPair: Unify[MKPair] = implicitly[Unify[Pair[MKData, MKData]]].asInstanceOf[Unify[MKPair]]

type mkMap = Mapping[MKData, MKData]

sealed trait MKMap

final class MKMapEmpty extends MappingEmpty[MKData, MKData] with MKMap

final class MKMapCons(key: VarOr[MKData], value: VarOr[MKData], tail: VarOr[mkMap]) extends MappingNonEmpty[MKData, MKData](key, value, tail.asInstanceOf) with MKMap

implicit val U$MKMap: Unify[MKMap] = implicitly[Unify[Mapping[MKData, MKData]]].asInstanceOf

enum MKThunkKind derives Unify :
  case Top
  case Bind
  case MPlus

val U$MKThunkKind: Unify[MKThunkKind] = implicitly[Unify[MKThunkKind]]

final case class MKThunk(kind: VarOr[MKThunkKind], xs: VarOr[Vector[VarOr[MKData]]]) derives Unify

val U$MKThunk: Unify[MKThunk] = implicitly[Unify[MKThunk]]

sealed trait MKGoal derives Unify

final case class MKGoalEq(u: VarOr[MKData], v: VarOr[MKData], env: VarOr[MKMap]) extends MKGoal derives Unify

final case class MKGoalCallFresh(f: VarOr[MKData], env: VarOr[MKMap]) extends MKGoal derives Unify

final case class MKGoalConj(g1: VarOr[MKData], g2: VarOr[MKData], env: VarOr[MKMap]) extends MKGoal derives Unify

final case class MKGoalDisj(g1: VarOr[MKData], g2: VarOr[MKData], env: VarOr[MKMap]) extends MKGoal derives Unify

final case class MKGoalTop(rand: VarOr[MKData], env: VarOr[MKMap]) extends MKGoal derives Unify

final case class MKRec(x: VarOr[MKData], exp: VarOr[MKData]) derives Unify

final case class MKReg(x: VarOr[MKData]) derives Unify

def list(xs: VarOr[MKData]*): VarOr[MKData] = xs.foldRight[VarOr[MKData]](())(cons)

def MKMapo(x: VarOr[MKData]): Rel[MKMap] = x.cast[MKMap]
