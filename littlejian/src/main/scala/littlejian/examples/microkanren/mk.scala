package littlejian.examples.microkanren

import littlejian._
import littlejian.data._

type MKData = MKVar | Unit | String | MKPair | MKGoal

implicit val U$MKData: Unifier[MKData] = U$Union(U$Union[MKVar, Unit, String, MKPair], U$MKGoal)

final case class MKVar(id: VarOr[Nat]) extends Product1[VarOr[Nat]]

implicit val U$MKVar: Unifier[MKVar] = U$Product

final case class MKPair(a: VarOr[MKData], b: VarOr[MKData]) extends Product2[VarOr[MKData], VarOr[MKData]]

implicit val U$MKPair: Unifier[MKPair] = U$Product

type MKGoal = (MKGoalEq | MKGoalCallFresh) | (MKGoalConj | MKGoalDisj) | MKGoalTop
implicit val U$MKGoal: Unifier[MKGoal] = U$Union(U$Union[MKGoalEq, MKGoalCallFresh], U$Union[MKGoalConj, MKGoalDisj], U$MKGoalTop)

final case class MKGoalEq(u: VarOr[MKData], v: VarOr[MKData], env: VarOr[MKData]) extends Product3[VarOr[MKData], VarOr[MKData], VarOr[MKData]]

implicit val U$MKGoalEq: Unifier[MKGoalEq] = U$Product

final case class MKGoalCallFresh(f: VarOr[MKData], env: VarOr[MKData]) extends Product2[VarOr[MKData], VarOr[MKData]]

implicit val U$MKGoalCallFresh: Unifier[MKGoalCallFresh] = U$Product

final case class MKGoalConj(g1: VarOr[MKData], g2: VarOr[MKData], env: VarOr[MKData]) extends Product3[VarOr[MKData], VarOr[MKData], VarOr[MKData]]

implicit val U$MKGoalConj: Unifier[MKGoalConj] = U$Product

final case class MKGoalDisj(g1: VarOr[MKData], g2: VarOr[MKData], env: VarOr[MKData]) extends Product3[VarOr[MKData], VarOr[MKData], VarOr[MKData]]

implicit val U$MKGoalDisj: Unifier[MKGoalDisj] = U$Product

final case class MKGoalTop(rand: VarOr[MKData], env: VarOr[MKData]) extends Product2[VarOr[MKData], VarOr[MKData]]

implicit val U$MKGoalTop: Unifier[MKGoalTop] = U$Product