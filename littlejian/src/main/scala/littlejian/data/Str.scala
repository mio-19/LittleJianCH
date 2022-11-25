package littlejian.data

import littlejian.*

import scala.language.implicitConversions

val I$Str = implicitly[Inspect[Str]]

final case class Str(xs: LList[Character]) derives Unify, Inspect, DeepWalk {
  override def toString: String = xs.getStrings match {
    case s: String => s"Str($s)"
    case (s, xs) => try {
      "\"" + xs.map(x => {
        val result = x.toString
        if (result.length != 1) throw new UnsupportedOperationException()
        result
      }).mkString("") + "\"" // TODO: support escape
    } catch {
      case _: UnsupportedOperationException => s"Str($s)"
    }
  }
}

object Str {
  def apply(xs: String): Str = new Str(LList.from(xs.toList.map(Character(_))))
}

implicit def StrPack(x: String): Str = Str(x)