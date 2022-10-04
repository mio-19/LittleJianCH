package littlejian.data

import littlejian._

sealed trait Nat

implicit val U$Nat: Unifier[Nat] = U$Union[Zero.type, Succ].asInstanceOf[Unifier[Nat]]

case object Zero extends Nat

implicit val U$Zero: Unifier[Zero.type] = equalUnifier

final case class Succ(prev: VarOr[Nat]) extends Nat with Product1[VarOr[Nat]]

implicit val U$Succ: Unifier[Succ] = U$Product

object Nat {
  def from(n: Int): Nat =
    if(n<0) throw new IllegalArgumentException("n must be non-negative")
    else if(n==0) Zero
    else Succ(from(n-1))
}