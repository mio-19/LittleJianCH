package littlejian.data

import littlejian._

sealed trait Character

object Character {
  def apply(chr: Int16): Character = Chr16(chr)

  def apply(code: Short): Character = ChrShort(code)

  def from(c: Char): Character = ChrShort.from(c)
}

final case class Chr16(chr: Int16) extends Character derives Unify, DeepWalk {
  override def toString: String = {
    val i = chr.toString
    try {
      Integer.parseInt(i).toChar.toString
    } catch {
      case _: NumberFormatException => s"Character(${i})"
    }
  }
}

object Chr16 {
  def from(n: Int16): Chr16 = Chr16(n)

  def from(n: Short): Chr16 = Chr16(Int16.from(n))

  def from(c: Char): Chr16 = Chr16(Int16.from(c.toShort))
}

final case class ChrShort(code: Short) extends Character derives Unify, DeepWalk {
  override def toString: String = {
    val i = code.toString
    try {
      java.lang.Short.toUnsignedInt(code).toChar.toString
    } catch {
      case _: NumberFormatException => s"Character(${i})"
    }
  }
}

object ChrShort {
  def from(c: Char): ChrShort = ChrShort(c.toShort)
}

implicit val U$Character: Unify[Character] = (x, y) => (x, y) match {
  case (Chr16(a), Chr16(b)) => a.unify(b)
  case (ChrShort(a), ChrShort(b)) => a.unify(b)
  case (Chr16(a), ChrShort(b)) => a.unify(Int16.from(b))
  case (ChrShort(a), Chr16(b)) => Int16.from(a).unify(b)
}