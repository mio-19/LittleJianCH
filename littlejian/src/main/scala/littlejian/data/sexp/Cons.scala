package littlejian.data.sexp

import littlejian._
import littlejian.ext._
import littlejian.data._

import scala.annotation.tailrec

type SExp0[SExp] = Pair[SExp, SExp] | Unit | String
type SExp = SExp0[Fix[SExp0]]

object SExp {
  def parse(s: String): SExp = {
    val (rest, r) = doParse(s)
    val s2 = rest.strip()
    if(s2.isEmpty) r else throw new Exception(s"parse error: remaining $s2")
  }

  private def doParse(s: String): (String, SExp) = {
    val (s1, s2) = s.strip().span(x=> x.isLetterOrDigit || x == '_' || x == '-')
    if(s1.nonEmpty) return (s2, s1)
    if(s2.isEmpty) throw new Exception("Invalid SExp")
    val c = s2.head
    val s3 = s2.tail
    c match {
      case '(' => {
        val xs = Seq.newBuilder[SExp]
        var rest = s3
        while(!(rest.isEmpty || rest.head == ')' || rest.head == '.')){
          val (s4, x) = doParse(rest)
          xs += x
          rest = s4.strip()
        }
        rest.head match {
          case ')' => (rest.tail, convertList(xs.result()))
          case '.' => {
            val (s4, x) = doParse(rest.tail)
            val s5 = s4.strip()
            if(s5.head != ')') throw new Exception("Invalid SExp")
            (s5.tail, convertListDot(xs.result(), x))
          }
        }
      }
      case '\'' => {
        val (s4, x) = doParse(s3)
        (s4, list("quote", x))
      }
    }
  }
}

implicit val U$SExp: Unifier[SExp] = U$Union(U$Pair, U$Unit, U$String)

implicit val I$SExp: Inspector[SExp] = I$Union(I$Pair, I$Unit, I$String)

private def consDot(xs: Vector[VarOr[SExp]], next: Var[SExp] | String): String =
  if (xs.isEmpty)
    next.toString
  else
    s"(${xs.map(_.toString).mkString(" ")} . ${next.toString})"

object Cons {
  def apply(a: VarOr[SExp], d: VarOr[SExp]): SExp = Pair(a, d)
  def unapply(s: VarOr[SExp]): Option[(VarOr[SExp], VarOr[SExp])] = s match {
    case Pair(a, d) => Some((a, d))
    case _ => None
  }
}

def cons(a: VarOr[SExp], d: VarOr[SExp]): SExp = Pair(a, d)

def car(s: VarOr[SExp]): VarOr[SExp] = s match {
  case Cons(a, _) => a
  case _ => throw new Exception(s"car: not a pair: $s")
}

private def convertList(xs: Seq[VarOr[SExp]]): SExp = if (xs.isEmpty) () else cons(xs.head, convertList(xs.tail))
private def convertListDot(xs: Seq[VarOr[SExp]]): VarOr[SExp] = {
  val head = xs.head
  val tail = xs.tail
  if(tail.isEmpty) head else cons(head, convertListDot(tail))
}
private def convertListDot(xs: Seq[VarOr[SExp]], x: SExp): SExp = convertListDot(xs :+ x).asInstanceOf[SExp]
object list {
  def apply(xs: VarOr[SExp]*): SExp = convertList(xs)
  def unapplySeq(x: VarOr[SExp]): Option[Seq[VarOr[SExp]]] = x match {
    case Cons(a, d) => unapplySeq(d).map(a +: _)
    case () => Some(Seq())
    case _ => None
  }
}
def listDot(xs: VarOr[SExp]*) = convertListDot(xs)

def mapo(f: VarOr[SExp] => Rel[SExp], xs: VarOr[SExp]): Rel[SExp] = conde(
  begin(xs === (), ()),
  for {
    (head, tail) <- xs.is[SExp, SExp](cons.apply)
    result <- cons.apply call(f(head), mapo(f, tail))
  } yield result
)
