package littlejian.data.sexp

import littlejian.*

import scala.annotation.tailrec

type SExp = Cons | Unit | String

private def consDot(xs: Vector[VarOr[SExp]], next: Var[SExp] | String): String = if (xs.isEmpty) next.toString else s"(${xs.map(_.toString).mkString(" ")} . ${next.toString})"

@tailrec
private def consToString(xs: Vector[VarOr[SExp]], next: VarOr[SExp]): String = next match {
  case Cons(a, d) => consToString(xs ++ Seq(a), d)
  case () => s"(${xs.map(_.toString).mkString(" ")})"
  case v: Var[SExp] => PrettyPrintContext.get match {
    case Some(context) => context.subst.getOption(v) match {
      case Some(box) => consToString(xs, box.x)
      case None => consDot(xs, v)
    }
    case None => consDot(xs, v)
  }
  case s: String => consDot(xs, s)
}

final case class Cons(a: VarOr[SExp], d: VarOr[SExp]) {
  override def toString: String = consToString(Vector(a), d)
}

implicit val U$Cons: Unifier[Cons] = (x, y) => for {
  _ <- x.a.unify(y.a)
  _ <- x.d.unify(y.d)
} yield ()

def cons(a: VarOr[SExp], d: VarOr[SExp]): SExp = Cons(a, d)

private def convertList(xs: Seq[VarOr[SExp]]): SExp = if (xs.isEmpty) () else cons(xs.head, convertList(xs.tail))
def list(xs: VarOr[SExp]*) = convertList(xs)


implicit val U$SExp: Unifier[SExp] = U$Or(U$Cons, U$Or(U$Unit, U$String))
