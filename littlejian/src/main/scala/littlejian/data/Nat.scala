package littlejian.data

import littlejian._
import littlejian.ext._

enum Nat derives Unifier :
  case Zero
  case Succ(prev: VarOr[Nat])

object Nat {
  def from(n: Int): Nat =
    if (n < 0) throw new IllegalArgumentException("n must be non-negative")
    else if (n == 0) Zero
    else Succ(from(n - 1))
}

implicit class VarOrNatOps[T](self: VarOr[Nat]) {
  def +(other: VarOr[Nat]): Rel[Nat] = conde(
    (self === Nat.Zero) >> other,
    for {
      prev <- self.is[Nat](Nat.Succ(_))
      sum <- prev + other
    } yield sum)

  def -(other: VarOr[Nat]): Rel[Nat] = for {
    result <- fresh[Nat]
    _ <- result + other === self
  } yield result

  def *(other: VarOr[Nat]): Rel[Nat] = conde(
    (self === Nat.Zero) >> Nat.Zero,
    (other === Nat.Zero) >> Nat.Zero,
    for {
      prev <- self.is[Nat](Nat.Succ(_))
      product <- prev * other
      sum <- product + other
    } yield sum)

  def >(other: VarOr[Nat]): Rel[Boolean] = conde(
    (self === Nat.Zero) >> false,
    for {
      prev <- self.is[Nat](Nat.Succ(_))
      otherPrev <- other.is[Nat](Nat.Succ(_))
      result <- prev > otherPrev
    } yield result,
    for {
      _ <- self.is[Nat](Nat.Succ(_))
      _ <- other === Nat.Zero
    } yield true)

  def <(other: VarOr[Nat]): Rel[Boolean] = other > self

  def >=(other: VarOr[Nat]): Rel[Boolean] = Nat.Succ(self) > other

  def <=(other: VarOr[Nat]): Rel[Boolean] = Nat.Succ(other) > self
}