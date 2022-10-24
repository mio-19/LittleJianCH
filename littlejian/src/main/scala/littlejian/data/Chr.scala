package littlejian.data

import littlejian._

final case class Chr(chr: Int16) extends Product1[Int16] {
  override def toString: String = {
    val i = chr.toString
    try {
      Integer.parseInt(i).toChar.toString
    } catch {
      case _: NumberFormatException => s"Chr(${i})"
    }
  }
}

object Chr {
  def from(n: Int16): Chr = Chr(n)

  def from(n: Short): Chr = Chr(Int16.from(n))

  def from(c: Char): Chr = Chr(Int16.from(c.toShort))
}

implicit val U$Chr: Unify[Chr] = U$Product