package littlejian.data

import littlejian.*

import scala.language.implicitConversions

implicit val I$LListChar: Inspect[LList[Character]] = Inspect.derived

final case class Str(xs: LList[Character]) derives Unify, Inspect {
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

val I$Str: Inspect[Str] = implicitly[Inspect[Str]]

object Str {
  def from(xs: LList[Character]): Str = Str(xs)

  def from(xs: String): Str = Str(LList.from(xs.toList.map(Character.from)))
}

implicit def StrPack(x: String): Str = Str.from(x)