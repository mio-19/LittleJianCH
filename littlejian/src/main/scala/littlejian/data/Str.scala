package littlejian.data

import littlejian._

final case class Str(xs: LList[Chr]) extends Product1[LList[Chr]] {
  override def toString: String = xs.getStrings match {
    case s: String => s"Str($s)"
    case (s, xs) => try {
      xs.map(x => {
        val result = x.toString
        if (result.length != 1) throw new UnsupportedOperationException()
        result
      }).mkString("")
    } catch {
      case _: UnsupportedOperationException => s"Str($s)"
    }
  }
}

object Str {
  def from(xs: LList[Chr]): Str = Str(xs)

  def from(xs: String): Str = Str(LList.from(xs.toList.map(Chr.from)))
}

implicit val U$Str: Unify[Str] = U$Product(U$LList(U$Chr))