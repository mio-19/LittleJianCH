package littlejian.data

import littlejian.*

import scala.language.implicitConversions

final case class Str(xs: LList[Character]) derives Unify, Inspect, DeepWalk {
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
  def from(xs: LList[Character]): Str = new Str(xs)

  def apply(xs: String): Str = new Str(LList.from(xs.toList.map(Character.from)))
}

implicit def StrPack(x: String): Str = Str(x)