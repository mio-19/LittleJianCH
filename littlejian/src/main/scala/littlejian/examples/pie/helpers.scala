package littlejian.examples.pie
import littlejian._
import littlejian.ext._
import littlejian.data.sexp._

def membero(x: VarOr[SExp], ls: VarOr[SExp]): Goal = {
  val a = hole[SExp]
  val d = hole[SExp]
  ls === cons(a, d) && (a === x || a =/= x && membero(x, d))
}

def notMembero(x: VarOr[SExp], ls: VarOr[SExp]): Goal = {
  ls === () || {
    val a = hole[SExp]
    val d = hole[SExp]
    ls === cons(a, d) && x =/= a && notMembero(x, d)
  }
}

def removeo(x: VarOr[SExp], ls: VarOr[SExp]): Rel[SExp] = conde(
  begin(ls === (), ()),
  {
    val d = hole[SExp]
    begin(ls === cons(x, d), removeo(x, d))
  },
  {
    val a = hole[SExp]
    val d = hole[SExp]
    for {
      _ <- ls === cons(a, d)
      _ <- x =/= a
      rest <- removeo(x, d)
    } yield cons(a, rest)
  }
)

def uniono(l1: VarOr[SExp], l2: VarOr[SExp]): Rel[SExp] = conde(
  begin(l1 === (), l2),
  {
    val a = hole[SExp]
    val d = hole[SExp]
    for {
      _ <- l1 === cons(a, d)
      tmp <- uniono(d, l2)
      rm <- removeo(a, tmp)
    } yield cons(a, rm)
  }
)

// symbols that might be confused with a function of 1 argument
def nonReservedPieFn(x: VarOr[SExp]): Goal = x =/= "add1" && x =/= "car" && x =/= "cdr" && x =/= "same"

// symbols that might be confused with variables
def reservedPieSymbol(x: VarOr[SExp]): Goal = x === "Atom" || x === "zero" || x === "Nat" || x === "sole" || x === "Trivial" || x === "U"
def nonReservedPieSymbol(x: VarOr[SExp]): Goal = x =/= "Atom" && x =/= "zero" && x =/= "Nat" && x =/= "sole" && x =/= "Trivial" && x =/= "U"

def applyΓ(Γ: VarOr[SExp], y: VarOr[SExp], τ: VarOr[SExp]): Goal = {
  val Γ2 = hole[SExp]
  conde(
    Γ === cons(list("free", y, τ), Γ2),
    {
      val v = hole[SExp]
      Γ === cons(list("def", y, v, τ), Γ2)
    },
    {
      val assoc = hole[SExp]
      assoc =/= "free" && assoc =/= "def" &&  Γ === cons(assoc, Γ2) && applyΓ(Γ2, y, τ)
    }
  )
}

def applyρ(ρ: VarOr[SExp], y: VarOr[SExp], v: VarOr[SExp]): Goal = ???