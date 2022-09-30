package littlejian.examples.pie
import littlejian._
import littlejian.ext._
import littlejian.data.sexp._

// https://github.com/bboskin/SFPW2018/blob/master/condp/helpers.rkt

/*
(defrel (membero x ls)
  (fresh (a d)
    (== ls `(,a . ,d))
    (conde
      [(== x a)]
      [(=/= x a)
       (membero x d)])))
*/
def membero(x: VarOr[SExp], ls: VarOr[SExp]): Goal = {
  val a = hole[SExp]
  val d = hole[SExp]
  ls === cons(a, d) && (a === x || a =/= x && membero(x, d))
}

/*
(defrel (not-membero x ls)
  (conde
    [(== ls '())]
    [(fresh (a d)
       (== ls `(,a . ,d))
       (=/= x a)
       (not-membero x d))]))
*/
def notMembero(x: VarOr[SExp], ls: VarOr[SExp]): Goal = {
  ls === () || {
    val a = hole[SExp]
    val d = hole[SExp]
    ls === cons(a, d) && x =/= a && notMembero(x, d)
  }
}

/*
(defrel (removo x ls o)
  (conde
    [(== ls '()) (== o '())]
    [(fresh (d)
       (== ls `(,x . ,d))
       (removo x d o))]
    [(fresh (a d o^)
       (=/= x a)
       (== ls `(,a . ,d))
       (removo x d o^)
       (== o `(,a . ,o^)))]))
*/
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

/*
(defrel (uniono l1 l2 o)
  (conde
    [(== l1 '()) (== o l2)]
    [(fresh (a d o^ rm)
       (== l1 `(,a . ,d))
       (uniono d l2 o^)
       (removo a o^ rm)
       (== o `(,a . ,rm)))]))
*/
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
/*
(defrel (non-reserved-Pie-fn s)
  (fresh ()
    (=/= s 'add1)
    (=/= s 'car)
    (=/= s 'cdr)
    (=/= s 'same)))
*/
def nonReservedPieFn(x: VarOr[SExp]): Goal = x =/= "add1" && x =/= "car" && x =/= "cdr" && x =/= "same"

// symbols that might be confused with variables
/*
(defrel (reserved-Pie-symbol s)
  (symbolo s)
  (conde
    [(== s 'Atom)]
    [(== s 'zero)]
    [(== s 'Nat)]
    [(== s 'sole)]
    [(== s 'Trivial)]
    [(== s 'U)]))
*/
def reservedPieSymbol(x: VarOr[SExp]): Goal = x === "Atom" || x === "zero" || x === "Nat" || x === "sole" || x === "Trivial" || x === "U"
def nonReservedPieSymbol(x: VarOr[SExp]): Goal = x =/= "Atom" && x =/= "zero" && x =/= "Nat" && x =/= "sole" && x =/= "Trivial" && x =/= "U"

/*
(defrel (apply-Γ Γ y τ)
  (conde
   [(fresh (Γ^) (== Γ `((free ,y ,τ) . ,Γ^)))]
   [(fresh (Γ^ v) (== Γ `((def ,y ,v ,τ) . ,Γ^)))]
   [(fresh (assoc Γ^)
      (=/= assoc 'free)
      (=/= assoc 'def)
      (== Γ `(,assoc . ,Γ^))
      (apply-Γ Γ^ y τ))]))
*/
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

/*
(defrel (apply-ρ ρ y v)
  (conde
   [(fresh (ρ^ τ) (== ρ `((def ,y ,v ,τ) . ,ρ^)))]
   [(fresh (ρ^) (== ρ `((val ,y ,v) . ,ρ^)))]
   [(fresh (assoc ρ^)
      (=/= assoc 'val)
      (=/= assoc 'def)
      (== ρ `(,assoc . ,ρ^))
      (apply-ρ ρ^ y v))]))
*/
def applyρ(ρ: VarOr[SExp], y: VarOr[SExp], v: VarOr[SExp]): Goal = conde(
  {
    val ρ2 = hole[SExp]
    val τ = hole[SExp]
    ρ === cons(list("def", y, v, τ), ρ2)
  },
  {
    val ρ2 = hole[SExp]
    ρ === cons(list("val", y, v), ρ2)
  },
  {
    val assoc = hole[SExp]
    val ρ2 = hole[SExp]
    assoc =/= "val" && assoc =/= "def" && ρ === cons(assoc, ρ2) && applyρ(ρ2, y, v)
  }
)

/*
(defrel (extend-Γ Γ y τ new-Γ)
  (== new-Γ `((free ,y ,τ) . ,Γ)))
*/
def extendΓ(Γ: VarOr[SExp], y: VarOr[SExp], τ: VarOr[SExp]): VarOr[SExp] = cons(list("free", y, τ), Γ)

/*
(defrel (extend-ρ ρ y v new-ρ)
  (== new-ρ `((val ,y ,v) . ,ρ)))
*/
def extendρ(ρ: VarOr[SExp], y: VarOr[SExp], v: VarOr[SExp]): VarOr[SExp] = cons(list("val", y, v), ρ)

/*
(defrel (extend-env ρ y v τ new-ρ)
  (== new-ρ `((def ,y ,v ,τ) . ,ρ)))
*/
def extendEnv(ρ: VarOr[SExp], y: VarOr[SExp], v: VarOr[SExp], τ: VarOr[SExp]): VarOr[SExp] = cons(list("def", y, v, τ), ρ)

/*
(defrel (free-in-Γ x Γ)
  (conde
    [(== Γ '())]
    [(fresh (tag y d Γ^)
       (== Γ `((,tag ,y . ,d) . ,Γ^))
       (=/= x y)
       (free-in-Γ x Γ^))]))
*/
def freeInΓ(x: VarOr[SExp], Γ: VarOr[SExp]): Goal = conde(
  Γ === (),
  {
    val tag = hole[SExp]
    val y = hole[SExp]
    val d = hole[SExp]
    val Γ2 = hole[SExp]
    Γ === cons(listDot(tag, y, d), Γ2) && x =/= y && freeInΓ(x, Γ2)
  }
)

/*
(defrel (free-in-ρ x ρ)
  (conde
    [(== ρ '())]
    [(fresh (name τ ρ^)
       (== ρ `((free ,name ,τ) . ,ρ^))
       (free-in-ρ x ρ^))]
    [(fresh (tag name d ρ^)
       (== ρ `((,tag ,name . ,d) . ,ρ^))
       (=/= tag 'free)
       (=/= name x)
       (free-in-ρ x ρ^))]))
*/
def freeInρ(x: VarOr[SExp], ρ: VarOr[SExp]): Goal = conde(
  ρ === (),
  {
    val name = hole[SExp]
    val τ = hole[SExp]
    val ρ2 = hole[SExp]
    ρ === cons(list("free", name, τ), ρ2) && freeInρ(x, ρ2)
  },
  {
    val tag = hole[SExp]
    val name = hole[SExp]
    val d = hole[SExp]
    val ρ2 = hole[SExp]
    ρ === cons(listDot(tag, name, d), ρ2) && tag =/= "free" && name =/= x && freeInρ(x, ρ2)
  }
)

// Variable freshening
/*
(defrel (just-names Γ names)
  (conde
   [(== Γ '()) (== names '())]
   [(fresh (x v t Γ^ o)
           (conde
            [(== Γ `((def ,x ,v ,t) . ,Γ^))
             (just-names Γ^ o)
             (== names `(,x . ,o))]
            [(== Γ `((free ,x ,t) . ,Γ^))
             (just-names Γ^ o)
             (== names `(,x . ,o))]
            [(== Γ `((val ,x ,v) . ,Γ^))
             (just-names Γ^ o)
             (== names `(,x . ,o))]))]))
*/
def justNames(Γ: VarOr[SExp]): Rel[SExp] = conde(
  begin(Γ === (), ()),
  {
    val x = hole[SExp]
    val v = hole[SExp]
    val t = hole[SExp]
    val Γ2 = hole[SExp]
    conde(
      for {
        _ <- Γ === cons(list("def", x, v, t), Γ2)
        o <- justNames(Γ2)
      } yield cons(x, o),
      for {
        _ <- Γ === cons(list("free", x, t), Γ2)
        o <- justNames(Γ2)
      } yield cons(x, o),
      for {
        _ <- Γ === cons(list("val", x, v), Γ2)
        o <- justNames(Γ2)
      } yield cons(x, o)
    ) : Rel[SExp]
  }
)

/*
(define (add-* x)
  (string->symbol
   (string-append (symbol->string x)
                  "*")))
*/
def addStar(x: String): String = x + "*"

/*
(defrel (fresh/aux x used name)
  (condu
   [(membero x used) (fresh/aux (add-* x) used name)]
   [(== x name)]))
*/
/*
def freshAux(x: String, used: VarOr[SExp], name: VarOr[SExp]): Goal = condu(
  membero(x, used) && freshAux(addStar(x), used, name),
  x === name
)
*/