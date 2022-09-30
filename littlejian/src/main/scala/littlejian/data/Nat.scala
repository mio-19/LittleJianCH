package littlejian.data

import littlejian._

type Nat = Zero.type | Succ

implicit val U$Nat: Unifier[Nat] = U$Union[Zero.type, Succ]

case object Zero

implicit val U$Zero: Unifier[Zero.type] = equalUnifier

final case class Succ(prev: VarOr[Nat]) extends Product1[VarOr[Nat]]

implicit val U$Succ: Unifier[Succ] = U$Product