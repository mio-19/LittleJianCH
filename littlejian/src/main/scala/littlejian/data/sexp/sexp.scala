package littlejian.data.sexp

import littlejian.*
import littlejian.data.*
import littlejian.ext.*

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.runtime.BoxedUnit

type SExp = Cons | Unit | String | BigDecimal | SExpVector | SExpLambda

final case class SExpVector(v: Vector[VarOr[SExp]]) derives Unify, Inspect {
  override def toString: String = s"#(${v.mkString(" ")})"
}

val I$SExpVector = implicitly[Inspect[SExpVector]]

final case class SExpLambda(fn: Seq[VarOr[SExp]] => VarOr[SExp])

implicit val U$SExpLambda: Unify[SExpLambda] = new AtomUnify[SExpLambda]() {}
implicit val I$SExpLambda: Inspect[SExpLambda] = (rec, self, x) => InspectResult(self == x)

implicit def sExpVector(v: Vector[_ <: VarOr[SExp]]): SExpVector = SExpVector(v)

implicit def sExpLambda(fn: ((xs: Seq[_ <: VarOr[SExp]]) => VarOr[SExp])): SExpLambda = SExpLambda(fn)

implicit val U$SExp: Unify[SExp] = U$Union[Cons, BoxedUnit, String, BigDecimal, SExpVector, SExpLambda].asInstanceOf

implicit val I$SExp: Inspect[SExp] = I$Union(I$Cons, I$BoxedUnit, I$String, I$BigDecimal, I$SExpVector, I$SExpLambda).asInstanceOf

object SExp {
  def parse(s: String): SExp = {
    val (rest, r) = doParse(s)
    val s2 = rest.strip()
    if (s2.isEmpty) r else throw new Exception(s"parse error: remaining $s2")
  }

  val allowedSymbols = Set('_', '-', '.')

  private def doParse(s: String): (String, SExp) = {
    val (s1, s2) = s.strip().span(x => x.isLetterOrDigit || allowedSymbols.contains(x))
    if (s1.nonEmpty) {
      return if (s1.apply(0).isDigit) {
        try {
          (s2, BigDecimal(s1))
        } catch {
          case e: NumberFormatException => throw new IllegalArgumentException("Invalid Number", e)
        }
      } else {
        (s2, s1)
      }
    }
    if (s2.isEmpty) throw new IllegalArgumentException("Invalid SExp")
    val c = s2.head
    val s3 = s2.tail
    c match {
      case begin@('(' | '[') => {
        val end = if (begin == '(') ')' else ']'
        val xs = Seq.newBuilder[SExp]
        var rest = s3
        while (!(rest.isEmpty || rest.head == end || rest.head == '.')) {
          val (s4, x) = doParse(rest)
          xs += x
          rest = s4.strip()
        }
        rest.head match {
          case c if c == end => (rest.tail, convertList(xs.result()))
          case '.' => {
            val (s4, x) = doParse(rest.tail)
            val s5 = s4.strip()
            if (s5.head != end) throw new Exception("Invalid SExp")
            (s5.tail, convertListDot(xs.result(), x))
          }
        }
      }
      case '\'' => {
        val (s4, x) = doParse(s3)
        (s4, list("quote", x))
      }
      case '`' => {
        val (s4, x) = doParse(s3)
        (s4, list("quasiquote", x))
      }
      case ',' => {
        if (s3.head == '@') {
          val (s4, x) = doParse(s3.tail)
          (s4, list("unquote-splicing", x))
        } else {
          val (s4, x) = doParse(s3)
          (s4, list("unquote", x))
        }
      }
      case '#' => doParse(s3) match {
        case (s4, list(xs*)) => (s4, SExpVector(xs.toVector))
        case _ => throw new Exception("Invalid SExp")
      }
    }
  }
}

final class Cons(a: VarOr[SExp], d: VarOr[SExp]) extends Pair[SExp, SExp](a, d) {
}

object Cons {
  def unapply(x: VarOr[SExp]): Option[(VarOr[SExp], VarOr[SExp])] = x match {
    case Pair(a, b) => Some(a.asInstanceOf, b.asInstanceOf)
    case _ => None
  }
}

implicit val U$Cons: Unify[Cons] = implicitly[Unify[Pair[SExp, SExp]]].asInstanceOf[Unify[Cons]]

implicit val I$Cons: Inspect[Cons] = implicitly[Inspect[Pair[SExp, SExp]]].asInstanceOf[Inspect[Cons]]

def cons(a: VarOr[SExp], d: VarOr[SExp]): SExp = Cons(a, d)
def car(x: VarOr[SExp]): VarOr[SExp] = x match {
  case Pair(v, _) => v
  case _ => throw new IllegalArgumentException("car: not a cons cell")
}

private def convertList(xs: Seq[VarOr[SExp]]): SExp = if (xs.isEmpty) () else cons(xs.head, convertList(xs.tail))
private def convertListDot(xs: Seq[VarOr[SExp]]): VarOr[SExp] = {
  val head = xs.head
  val tail = xs.tail
  if (tail.isEmpty) head else cons(head, convertListDot(tail))
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
    (head, tail) <- xs.is(cons)
    result <- cons call(f(head), mapo(f, tail))
  } yield result
)
