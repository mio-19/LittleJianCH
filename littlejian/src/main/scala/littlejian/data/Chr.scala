package littlejian.data

import littlejian._

final case class Chr(chr: Int16) extends Product1[Int16] {
  def toInt16: Int16 = chr
}

object Chr {
  def from(n: Int16): Chr = Chr(n)

  def from(n: Short): Chr = Chr(Int16.from(n))

  def from(c: Char): Chr = Chr(Int16.from(c.toShort))
}

implicit val U$Chr: Unifier[Chr] = U$Product