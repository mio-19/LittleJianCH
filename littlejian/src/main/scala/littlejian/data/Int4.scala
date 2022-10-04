package littlejian.data

import littlejian._

final case class Int4(bit0: VarOr[Boolean], bit1: VarOr[Boolean], bit2: VarOr[Boolean], bit3: VarOr[Boolean]) extends Product4[VarOr[Boolean], VarOr[Boolean], VarOr[Boolean], VarOr[Boolean]] {

}
implicit val U$Int4: Unifier[Int4] = U$Product


final case class Int8(lo: Int4, hi: Int4) extends Product2[Int4, Int4] {
}


implicit val U$Int8: Unifier[Int8] = U$Product

final case class Int16(lo: Int8, hi: Int8) extends Product2[Int8, Int8] {

}

implicit val U$Int16: Unifier[Int16] = U$Product

object Int16 {
  def from(x: Short): Int16 = ???
}

final case class Int32(lo: Int16, hi: Int16) extends Product2[Int16, Int16] {
  def +(other: Int32): Int32 = ???

  def -(other: Int32): Int32 = ???

  def *(other: Int32): Int32 = ???

  def divUnsigned(other: Int32): Int32 = ???

  def /(other: Int32): Int32 = ???

  def modUnsigned(other: Int32): Int32 = ???

  def %(other: Int32): Int32 = ???
}

object Int32 {
  def from(n: Int): Int32 = ???
}
