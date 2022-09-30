package littlejian.data.sexp

import littlejian._

import scala.annotation.tailrec

type SExp = Cons | Unit | String

implicit val U$SExp: Unifier[SExp] = U$Union[Cons, Unit, String]

implicit val I$SExp: Inspector[SExp] = I$Union(I$Cons, I$Unit, I$String)

private def consDot(xs: Vector[VarOr[SExp]], next: Var[SExp] | String): String =
  if (xs.isEmpty)
    next.toString
  else
    s"(${xs.map(_.toString).mkString(" ")} . ${next.toString})"

@tailrec
private def consToString(xs: Vector[VarOr[SExp]], next: VarOr[SExp]): String = next match {
  case Cons(a, d) => consToString(xs ++ Seq(a), d)
  case () => s"(${xs.map(_.toString).mkString(" ")})"
  case v: Var[SExp] => prettyPrintContext.get match {
    case Some(context) => context.subst.getOption(v) match {
      case Some(x) => consToString(xs, x)
      case None => consDot(xs, v)
    }
    case None => consDot(xs, v)
  }
  case s: String => consDot(xs, s)
}

final case class Cons(a: VarOr[SExp], d: VarOr[SExp]) extends Product2[VarOr[SExp], VarOr[SExp]] {
  override def toString: String = consToString(Vector(a), d)
}
implicit val U$Cons: Unifier[Cons] = U$Product

implicit val I$Cons: Inspector[Cons] = {
  case Cons(a, d) => Seq(WithInspector(a), WithInspector(d))
}

def cons(a: VarOr[SExp], d: VarOr[SExp]): SExp = Cons(a, d)

private def convertList(xs: Seq[VarOr[SExp]]): SExp = if (xs.isEmpty) () else cons(xs.head, convertList(xs.tail))
private def convertListDot(xs: Seq[VarOr[SExp]]): VarOr[SExp] = {
  val head = xs.head
  val tail = xs.tail
  if(tail.isEmpty) head else cons(head, convertListDot(tail))
}
object list {
  def apply(xs: VarOr[SExp]*) = convertList(xs)
  def unapplySeq(x: VarOr[SExp]): Option[Seq[VarOr[SExp]]] = x match {
    case Cons(a, d) => unapplySeq(d).map(a +: _)
    case () => Some(Seq())
    case _ => None
  }
}
def listDot(xs: VarOr[SExp]*) = convertListDot(xs)

