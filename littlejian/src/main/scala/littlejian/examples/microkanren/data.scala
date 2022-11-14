package littlejian.examples.microkanren

import littlejian.*
import littlejian.data.*
import littlejian.ext.*

import scala.language.implicitConversions

type MKData = (Boolean | Unit | String | MKPair) | (MKVar | MKGoal | MKThunk | MKMap) | (MKRec | MKReg)

implicit val U$MKData: Unify[MKData] = U$Union[Boolean, Unit, String, MKPair, MKVar, MKGoal, MKThunk, MKMap, MKRec, MKReg]

final case class MKVar(id: VarOr[Nat]) extends Product1[VarOr[Nat]]

implicit val U$MKVar: Unify[MKVar] = U$Product

final class MKPair(a: VarOr[MKData], b: VarOr[MKData]) extends Pair[MKData, MKData](a, b)

def cons(a: VarOr[MKData], b: VarOr[MKData]): MKPair = new MKPair(a, b)

implicit val U$MKPair: Unify[MKPair] = implicitly[Unify[Pair[MKData, MKData]]].asInstanceOf[Unify[MKPair]]

type mkMap = Mapping[MKData, MKData]

sealed trait MKMap

final class MKMapEmpty extends MappingEmpty[MKData, MKData] with MKMap

final class MKMapCons(key: VarOr[MKData], value: VarOr[MKData], tail: VarOr[mkMap]) extends MappingNonEmpty[MKData, MKData](key, value, tail.asInstanceOf) with MKMap

implicit def VarOrMKMap2VarOrmkMap(x: VarOr[MKMap]): VarOr[mkMap] = x.asInstanceOf
implicit def VarOrmkMap2VarOrMKMap(x: VarOr[mkMap]): VarOr[MKMap] = x.asInstanceOf

implicit val U$MKMap: Unify[MKMap] = implicitly[Unify[Mapping[MKData, MKData]]].asInstanceOf

val U$MKThunk: Unify[MKThunk] = implicitly[Unify[MKThunk]]

enum MKThunk derives Unify:
  case Top(env: VarOr[mkMap], rand: VarOr[MKData], s: VarOr[MKData], c: VarOr[MKData])
  case Bind(xs: VarOr[MKData], g: VarOr[MKData])
  case MPlus(xs: VarOr[MKData], ys: VarOr[MKData])


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
