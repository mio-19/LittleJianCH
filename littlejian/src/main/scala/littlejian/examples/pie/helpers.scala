package littlejian.examples.pie

import littlejian.*
import littlejian.ext.*
import littlejian.data._
import littlejian.data.sexp.*

import scala.language.implicitConversions


implicit def SeqToSExp(ls: Seq[String]): SExp = list(ls *)
def walkStar(walker: Walker, x: VarOr[SExp]): VarOr[SExp] = walker(x) match {
  case () => ()
  case Pair(a, d) => Cons(walkStar(walker, a), walkStar(walker, d))
  case x: String => x
  case x: Var[_] => x
}

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
def membero(x: VarOr[SExp], ls: VarOr[SExp]): Goal = for {
  (a, d) <- ls.is(cons)
  _ <- (a === x || a =/= x && membero(x, d))
} yield ()

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
  ls === () || (for {
    (a, d) <- ls.is(cons)
    _ <- x =/= a && notMembero(x, d)
  } yield ())
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
  for {
    d <- fresh[SExp]
    _ <- ls === cons(x, d)
    o <- removeo(x, d)
  } yield o,
  for {
    (a, d) <- ls.is(cons)
    _ <- x =/= a
    o_ <- removeo(x, d)
  } yield cons(a, o_))

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
  for {
    (a, d) <- l1.is(cons)
    tmp <- uniono(d, l2)
    rm <- removeo(a, tmp)
  } yield cons(a, rm)
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
def applyΓ(Γ: VarOr[SExp], y: VarOr[SExp], τ: VarOr[SExp]): Goal = conde(
  for {
    Γ_ <- fresh[SExp]
    _ <- Γ === cons(list("free", y, τ), Γ_)
  } yield (),
  for {
    Γ_ <- fresh[SExp]
    v <- fresh[SExp]
    _ <- Γ === cons(list("def", y, v, τ), Γ_)
  } yield (),
  for {
    Γ_ <- fresh[SExp]
    assoc <- fresh[SExp]
    _ <- Γ === cons(assoc, Γ_)
    _ <- assoc =/= "free" && assoc =/= "def"
    _ <- applyΓ(Γ_, y, τ)
  } yield ())

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
  for {
    ρ_ <- fresh[SExp]
    τ <- fresh[SExp]
    _ <- ρ === cons(list("def", y, v, τ), ρ_)
  } yield (),
  for {
    ρ_ <- fresh[SExp]
    _ <- ρ === cons(list("val", y, v), ρ_)
  } yield (),
  for {
    ρ_ <- fresh[SExp]
    assoc <- fresh[SExp]
    _ <- ρ === cons(assoc, ρ_)
    _ <- assoc =/= "val" && assoc =/= "def"
    _ <- applyρ(ρ_, y, v)
  } yield ())

/*
(defrel (extend-Γ Γ y τ new-Γ)
  (== new-Γ `((free ,y ,τ) . ,Γ)))
*/
def extendΓ(Γ: VarOr[SExp], y: VarOr[SExp], τ: VarOr[SExp]): VarOr[SExp] = cons(list("free", y, τ), Γ)
def extendΓ(Γ: VarOr[SExp], y: VarOr[SExp], τ: VarOr[SExp], newΓ: VarOr[SExp]): Goal = newΓ === extendΓ(Γ, y, τ)

/*
(defrel (extend-ρ ρ y v new-ρ)
  (== new-ρ `((val ,y ,v) . ,ρ)))
*/
def extendρ(ρ: VarOr[SExp], y: VarOr[SExp], v: VarOr[SExp]): VarOr[SExp] = cons(list("val", y, v), ρ)
def extendρ(ρ: VarOr[SExp], y: VarOr[SExp], v: VarOr[SExp], o: VarOr[SExp]): Goal = o === extendρ(ρ, y, v)

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
  for {
    tag <- fresh[SExp]
    y <- fresh[SExp]
    d <- fresh[SExp]
    Γ_ <- fresh[SExp]
    _ <- Γ === cons(list(tag, y, d), Γ_)
    _ <- x =/= y
    _ <- freeInΓ(x, Γ_)
  } yield ())

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
  (for {
    (_, τ, ρ2) <- ρ.is((name: VarOr[SExp], τ: VarOr[SExp], ρ2: VarOr[SExp]) => cons(list("free", name, τ), ρ2))
    _ <- freeInρ(x, ρ2)
  } yield ()): Goal,
  (for {
    (tag, name, _, ρ2) <- ρ.is((tag: VarOr[SExp], name: VarOr[SExp], d: VarOr[SExp], ρ2: VarOr[SExp]) => cons(listDot(tag, name, d), ρ2))
    _ <- tag =/= "free" && name =/= x && freeInρ(x, ρ2)
  } yield ()): Goal
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
def justNames(Γ: VarOr[SExp], names: VarOr[SExp]): Goal = justNames(Γ)(names)
def justNames(Γ: VarOr[SExp]): Rel[SExp] = conde(
  for {
    _ <- Γ === ()
  } yield (),
  for {
    x <- fresh[SExp]
    v <- fresh[SExp]
    t <- fresh[SExp]
    Γ_ <- fresh[SExp]
    result <- conde(
      for {
        _ <- Γ === cons(list("def", x, v, t), Γ_)
        o <- justNames(Γ_)
      } yield cons(x, o) : SExp,
      for {
        _ <- Γ === cons(list("free", x, t), Γ_)
        o <- justNames(Γ_)
      } yield cons(x, o) : SExp,
      for {
        _ <- Γ === cons(list("val", x, v), Γ_)
        o <- justNames(Γ_)
      } yield cons(x, o) : SExp): Rel[SExp]
  } yield result)

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
def freshAux(x: String, used: VarOr[SExp], name: VarOr[SExp]): Goal = condu(
  (membero(x, used), freshAux(addStar(x), used, name)),
  (x === name, Goal.success))

/*
(defrel (freshen x used name)
  (condu
   [(membero x used) (fresh/aux 'var used name)]
   [(== x name)]))
*/
def freshen(x: VarOr[SExp], used: VarOr[SExp], name: VarOr[SExp]): Goal = condu(
  (membero(x, used), freshAux("var", used, name)),
  (x === name, Goal.success))

/*
(defrel (fresh-name o)
  (== o (gensym 'tmp)))
*/
def freshName(o: VarOr[SExp]): Goal = o === gensym("tmp")

// For relevance functions
/*
(define all-exprs
  '(var the zero sole Nat Trivial Atom U quote add1 app
        same λ cons car cdr ind-Nat ind-= Π Σ =))
(define symbol-exprs
  '(zero sole Atom Nat Trivial U))
(define non-symbol-exprs
  '(the quote add1 same λ cons car cdr ind-Nat ind-= Π Σ =))
(define ((exp-memv? ls) e)
  (and (pair? e) (memv (car e) ls)))
*/
val allExprs: Seq[String] = Seq("var", "the", "zero", "sole", "Nat", "Trivial", "Atom", "U", "quote", "add1", "app", "same", "λ", "cons", "car", "cdr", "ind-Nat", "ind-=", "Π", "Σ", "=")
val symbolExprs: Seq[String] = Seq("zero", "sole", "Atom", "Nat", "Trivial", "U")
val nonSymbolExprs: Seq[String] = Seq("the", "quote", "add1", "same", "λ", "cons", "car", "cdr", "ind-Nat", "ind-=", "Π", "Σ", "=")
def memv(v: VarOr[SExp], ls: VarOr[SExp]): Boolean = ls match {
  case Pair(car, cdr) => car == v || memv(v, cdr)
  case _ => false
}
def expMemvQ(ls: VarOr[SExp])(e: VarOr[SExp]): Boolean = e match {
  case Pair(car, cdr) => memv(car, ls)
  case _ => false
}
// (define simple? (λ (x) (memv x symbol-exprs)))
def simpleQ(x: String): Boolean = symbolExprs.contains(x)

/*
(define (get-constructors τ)
  (match τ
    ['Atom '(quote)]
    ['Trivial '(sole)]
    ['Nat '(zero add1)]
    ['U '(Trivial Atom Nat Π Σ =)]
    [`(Π ([,x ,A]) ,R) '(λ)]
    [`(Σ ([,x ,A]) ,D) '(cons)]
    [`(= ,X ,from ,to) '(same)]
    [else '()]))
*/
def getConstructors(τ: VarOr[SExp]): Seq[String] = τ match {
  case "Atom" => Seq("quote")
  case "Trivial" => Seq("sole")
  case "Nat" => Seq("zero", "add1")
  case "U" => Seq("Trivial", "Atom", "Nat", "Π", "Σ", "=")
  case list("Π", list(list(x, _)), _) => Seq("λ")
  case list("Σ", list(list(x, _)), _) => Seq("cons")
  case list("=", _, from, to) => Seq("same")
  case _ => Seq.empty
}