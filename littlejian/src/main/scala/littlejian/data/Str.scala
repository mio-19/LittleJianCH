package littlejian.data

import littlejian._

final case class Str(xs: LList[Chr]) extends Product1[LList[Chr]] {
  def toLList: LList[Chr] = xs
}

object Str {
  def from(xs: LList[Chr]): Str = Str(xs)

  def from(xs: String): Str = Str(LList.from(xs.toList.map(Chr.from)))
}

implicit val U$Str: Unifier[Str] = U$Product(U$LList(U$Chr))