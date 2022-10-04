package littlejian.data

import littlejian._

final case class Chr(chr: Nat) extends Product1[Nat] {
  def toNat: Nat = chr
}

object Chr {
  def from(n: Nat): Chr = Chr(n)

  def from(n: Int): Chr = Chr(Nat.from(n))
  
  def from(c: Char): Chr = Chr(Nat.from(c.toInt))
}

implicit val U$Chr: Unifier[Chr] = U$Product