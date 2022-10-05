package littlejian.data

import littlejian._
import littlejian.ext._

sealed trait Nat derives Unifier

case object Zero extends Nat derives Unifier {
  override def toString = "0"
}

final case class Succ(prev: VarOr[Nat]) extends Nat derives Unifier {
}

object Nat {
  def from(n: Int): Nat =
    if (n < 0) throw new IllegalArgumentException("n must be non-negative")
    else if (n == 0) Zero
    else Succ(from(n - 1))
}

implicit class VarOrNatOps[T](self: VarOr[Nat]) {
  def +(other: VarOr[Nat]): Rel[Nat] = conde(
    (self === Zero) >> other,
    for {
      prev <- self.is[Nat](Succ(_))
      sum <- prev + other
    } yield sum)

  def -(other: VarOr[Nat]): Rel[Nat] = for {
    result <- fresh[Nat]
    _ <- result + other === self
  } yield result

  def *(other: VarOr[Nat]): Rel[Nat] = conde(
    (self === Zero) >> Zero,
    (other === Zero) >> Zero,
    for {
      prev <- self.is[Nat](Succ(_))
      product <- prev * other
      sum <- product + other
    } yield sum)

  def >(other: VarOr[Nat]): Rel[Boolean] = conde(
    (self === Zero) >> false,
    for {
      prev <- self.is[Nat](Succ(_))
      otherPrev <- other.is[Nat](Succ(_))
      result <- prev > otherPrev
    } yield result,
    for {
      _ <- self.is[Nat](Succ(_))
      _ <- other === Zero
    } yield true)

  def <(other: VarOr[Nat]): Rel[Boolean] = other > self

  def >=(other: VarOr[Nat]): Rel[Boolean] = Succ(self) > other

  def <=(other: VarOr[Nat]): Rel[Boolean] = Succ(other) > self
}