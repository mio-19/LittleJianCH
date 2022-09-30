package littlejian.examples.pie

import littlejian._
import littlejian.ext._
import littlejian.data.sexp._

// https://github.com/bboskin/SFPW2018/blob/master/condp/normalize.rkt

/*
(defrel (normalizo Γ τ exp o)
  (fresh (v)
    (valofo Γ exp v)
    (read-backo Γ τ v o)))
*/
def normalizo(Γ: VarOr[SExp], τ: VarOr[SExp], exp: VarOr[SExp]): Rel[SExp] = for {
  v <- valofo(Γ, exp)
  o <- readBacko(Γ, τ, v)
} yield o

// Helpers for valofo

/*
(defrel (assign-simple tₑ tᵥ exp v)
  (== exp tₑ)
  (== v tᵥ))
*/
def assignSimple(te: VarOr[SExp], tv: VarOr[SExp], exp: VarOr[SExp], v: VarOr[SExp]): Goal = te === exp && tv === v

/*
(defrel (valof-the ρ exp v)
  (fresh (τ e e-v t-v)
    (== exp `(the ,τ ,e))
    (== v `(THE ,t-v ,e-v))
    (valofo ρ e e-v)
    (valofo ρ τ t-v)))
*/
def valofThe(ρ: VarOr[SExp], exp: VarOr[SExp], v: VarOr[SExp]): Goal = for {
  τ <- fresh[SExp]
  e <- fresh[SExp]
  ev <- fresh[SExp]
  tv <- fresh[SExp]
  _ <- exp === list("the", τ, e)
  _ <- v === list("THE", tv, ev)
  _ <- valofo(ρ, e, ev)
  _ <- valofo(ρ, τ, tv)
} yield ()

/*
(defrel (valof-neutral-var ρ exp v)
  (fresh (T)
    (== v `(NEU ,T (VAR ,exp)))
    (apply-Γ ρ exp T)))
*/
def valofNeutralVar(ρ: VarOr[SExp], exp: VarOr[SExp], v: VarOr[SExp]): Goal = for {
  T <- fresh[SExp]
  _ <- v === list("NEU", T, list("VAR", exp))
  _ <- applyΓ(ρ, exp, T)
} yield ()

/*
(defrel (valof-quote ρ exp v)
  (fresh (atom)
    (== exp `(quote ,atom))
    (== v `(ATOM ,atom))))
*/
def valofQuote(ρ: VarOr[SExp], exp: VarOr[SExp]): Rel[SExp] = for {
  atom <- fresh[SExp]
  _ <- exp === list("quote", atom)
} yield list("ATOM", atom)
def valofQuote(ρ: VarOr[SExp], exp: VarOr[SExp], v: VarOr[SExp]): Goal = valofQuote(ρ, exp)(v)

/*
(defrel (valof-Π ρ exp v)
  (fresh (x A D Ao)
    (== exp `(Π ([,x ,A]) ,D))
    (== v `(PI ,x ,Ao (CLOS ,ρ ,x ,D)))
    (valofo ρ A Ao)))
*/
def valofPi(ρ: VarOr[SExp], exp: VarOr[SExp], v: VarOr[SExp]): Goal = for {
  x <- fresh[SExp]
  A <- fresh[SExp]
  D <- fresh[SExp]
  Ao <- fresh[SExp]
  _ <- exp === list("Π", list(list(x, A)), D)
  _ <- v === list("PI", x, Ao, list("CLOS", ρ, x, D))
  _ <- valofo(ρ, A, Ao)
} yield ()

/*
(defrel (valof-λ ρ exp v)
  (fresh (x b)
    (== exp `(λ (,x) ,b))
    (symbolo x)
    (== v `(LAM ,x (CLOS ,ρ ,x ,b)))))
*/
def valofLam(ρ: VarOr[SExp], exp: VarOr[SExp], v: VarOr[SExp]): Goal = for {
  x <- fresh[SExp]
  b <- fresh[SExp]
  _ <- exp === list("λ", list(x), b)
  _ <- x.isType[String]
  _ <- v === list("LAM", x, list("CLOS", ρ, x, b))
} yield ()

/*
(defrel (valof-app ρ exp v)
  (fresh (rator rand rato rando)
    (== exp `(,rator ,rand))
    (valofo ρ rator rato)
    (valofo ρ rand rando)
    (do-appo rato rando v)))
*/
def valofApp(ρ: VarOr[SExp], exp: VarOr[SExp], v: VarOr[SExp]): Goal = for {
  rator <- fresh[SExp]
  rand <- fresh[SExp]
  rato <- fresh[SExp]
  rando <- fresh[SExp]
  _ <- exp === list(rator, rand)
  _ <- valofo(ρ, rator, rato)
  _ <- valofo(ρ, rand, rando)
  _ <- doAppo(rato, rando, v)
} yield ()

/*
(defrel (valof-closuro clo v ans)
  (fresh (ρ x e ρ^)
    (== clo `(CLOS ,ρ ,x ,e))
    (extend-ρ ρ x v ρ^)
    (valofo ρ^ e ans)))
*/
def valofClosuro(clo: VarOr[SExp], v: VarOr[SExp], ans: VarOr[SExp]): Goal = for {
  ρ <- fresh[SExp]
  x <- fresh[SExp]
  e <- fresh[SExp]
  ρ_ <- fresh[SExp]
  _ <- clo === list("CLOS", ρ, x, e)
  _ <- extendρ(ρ, x, v, ρ_)
  _ <- valofo(ρ_, e, ans)
} yield ()

/*
(defrel (do-appo f v o)
  (conde
   [(fresh (x c)
      (== f `(LAM ,x ,c))
      (valof-closuro c v o))]
   [(fresh (x A c ne T)
           (== f `(NEU (PI ,x ,A ,c) ,ne))
           (== o `(NEU ,T (N-APP (NEU (PI ,x ,A ,c) ,ne) ,v)))
           (valof-closuro c v T))]))
*/
def doAppo(f: VarOr[SExp], v: VarOr[SExp], o: VarOr[SExp]): Goal = conde(
  for {
    x <- fresh[SExp]
    c <- fresh[SExp]
    _ <- f === list("LAM", x, c)
    _ <- valofClosuro(c, v, o)
  } yield (),
  for {
    x <- fresh[SExp]
    A <- fresh[SExp]
    c <- fresh[SExp]
    ne <- fresh[SExp]
    T <- fresh[SExp]
    _ <- f === list("NEU", list("PI", x, A, c), ne)
    _ <- o === list("NEU", T, list("N-APP", list("NEU", list("PI", x, A, c), ne), v))
    _ <- valofClosuro(c, v, T)
  } yield ()
)

/*
(defrel (valof-Σ ρ exp v)
  (fresh (x A D Ao)
    (== exp `(Σ ([,x ,A]) ,D))
    (== v `(SIGMA ,x ,Ao (CLOS ,ρ ,x ,D)))
    (valofo ρ A Ao)))
*/
def valofSigma(ρ: VarOr[SExp], exp: VarOr[SExp], v: VarOr[SExp]): Goal = for {
  x <- fresh[SExp]
  A <- fresh[SExp]
  D <- fresh[SExp]
  Ao <- fresh[SExp]
  _ <- exp === list("Σ", list(list(x, A)), D)
  _ <- v === list("SIGMA", x, Ao, list("CLOS", ρ, x, D))
  _ <- valofo(ρ, A, Ao)
} yield ()

/*
(defrel (valof-cons ρ exp v)
  (fresh (a d a^ d^)
    (== exp `(cons ,a ,d))
    (== v `(CONS ,a^ ,d^))
    (valofo ρ a a^)
    (valofo ρ d d^)))
*/
def valofCons(ρ: VarOr[SExp], exp: VarOr[SExp], v: VarOr[SExp]): Goal = for {
  a <- fresh[SExp]
  d <- fresh[SExp]
  a_ <- fresh[SExp]
  d_ <- fresh[SExp]
  _ <- exp === list("cons", a, d)
  _ <- v === list("CONS", a_, d_)
  _ <- valofo(ρ, a, a_)
  _ <- valofo(ρ, d, d_)
} yield ()

/*
(defrel (valof-car ρ exp v)
  (fresh (pr pr^)
    (== exp `(car ,pr))
    (do-caro pr^ v)
    (valofo ρ pr pr^)))
*/
def valofCar(ρ: VarOr[SExp], exp: VarOr[SExp], v: VarOr[SExp]): Goal = for {
  pr <- fresh[SExp]
  pr_ <- fresh[SExp]
  _ <- exp === list("car", pr)
  _ <- doCaro(pr_, v)
  _ <- valofo(ρ, pr, pr_)
} yield ()

/*
(defrel (do-caro pr v)
  (conde
   [(fresh (a d)
           (== pr `(CONS ,a ,d))
           (== v a))]
   [(fresh (x A D ne)
           (== pr `(NEU (SIGMA ,x ,A ,D) ,ne))
           (== v `(NEU ,A (CAR (NEU (SIGMA ,x ,A ,D) ,ne)))))]))
*/
def doCaro(pr: VarOr[SExp], v: VarOr[SExp]): Goal = conde(
  for {
    a <- fresh[SExp]
    d <- fresh[SExp]
    _ <- pr === list("CONS", a, d)
    _ <- v === a
  } yield (),
  for {
    x <- fresh[SExp]
    A <- fresh[SExp]
    D <- fresh[SExp]
    ne <- fresh[SExp]
    _ <- pr === list("NEU", list("SIGMA", x, A, D), ne)
    _ <- v === list("NEU", A, list("CAR", list("NEU", list("SIGMA", x, A, D), ne)))
  } yield ()
)
/*
(defrel (valof-cdr ρ exp v)
  (fresh (pr pr^)
    (== exp `(cdr ,pr))
    (valofo ρ pr pr^)
    (do-cdro pr^ v)))
*/
def valofCdr(ρ: VarOr[SExp], exp: VarOr[SExp], v: VarOr[SExp]): Goal = for {
  pr <- fresh[SExp]
  pr_ <- fresh[SExp]
  _ <- exp === list("cdr", pr)
  _ <- valofo(ρ, pr, pr_)
  _ <- doCdro(pr_, v)
} yield ()
/*
(defrel (do-cdro pr v)
  (conde
   [(fresh (a d)
           (== pr `(CONS ,a ,d))
           (== v d))]
   [(fresh (x A D D^ ne a)
           (== pr `(NEU (SIGMA ,x ,A ,D) ,ne))
           (do-caro pr a)
           (valof-closuro D a D^)
           (== v `(NEU ,D^ (CDR (NEU (SIGMA ,x ,A ,D) ,ne)))))]))
*/
def doCdro(pr: VarOr[SExp], v: VarOr[SExp]): Goal = conde(
  for {
    a <- fresh[SExp]
    d <- fresh[SExp]
    _ <- pr === list("CONS", a, d)
    _ <- v === d
  } yield (),
  for {
    x <- fresh[SExp]
    A <- fresh[SExp]
    D <- fresh[SExp]
    D_ <- fresh[SExp]
    ne <- fresh[SExp]
    a <- fresh[SExp]
    _ <- pr === list("NEU", list("SIGMA", x, A, D), ne)
    _ <- doCaro(pr, a)
    _ <- valofClosuro(D, a, D_)
    _ <- v === list("NEU", D_, list("CDR", list("NEU", list("SIGMA", x, A, D), ne)))
  } yield ()
)
/*
(defrel (valof-add1 ρ exp v)
  (fresh (n nV)
    (== exp `(add1 ,n))
    (== v `(ADD1 ,nV))
    (valofo ρ n nV)))
*/
def valofAdd1(ρ: VarOr[SExp], exp: VarOr[SExp], v: VarOr[SExp]): Goal = for {
  n <- fresh[SExp]
  nV <- fresh[SExp]
  _ <- exp === list("add1", n)
  _ <- v === list("ADD1", nV)
  _ <- valofo(ρ, n, nV)
} yield ()
/*
(defrel (valof-ind-Nat ρ exp v)
  (fresh (t m τ ba s tV mV bV^ bV sV T)
    (== exp `(ind-Nat ,t ,m (the ,τ ,ba) ,s))
    (== bV `(THE ,T ,bV^))
    (valofo ρ t tV)
    (valofo ρ m mV)
    (valofo ρ ba bV^)
    (valofo ρ τ T)
    (valofo ρ s sV)
    (do-ind-Nat tV mV bV sV v)))
*/
def valofIndNat(ρ: VarOr[SExp], exp: VarOr[SExp], v: VarOr[SExp]): Goal = for {
  t <- fresh[SExp]
  m <- fresh[SExp]
  τ <- fresh[SExp]
  ba <- fresh[SExp]
  s <- fresh[SExp]
  tV <- fresh[SExp]
  mV <- fresh[SExp]
  bV_ <- fresh[SExp]
  bV <- fresh[SExp]
  sV <- fresh[SExp]
  T <- fresh[SExp]
  _ <- exp === list("ind-Nat", t, m, list("the", τ, ba), s)
  _ <- bV === list("THE", T, bV_)
  _ <- valofo(ρ, t, tV)
  _ <- valofo(ρ, m, mV)
  _ <- valofo(ρ, ba, bV_)
  _ <- valofo(ρ, τ, T)
  _ <- valofo(ρ, s, sV)
  _ <- doIndNat(tV, mV, bV, sV, v)
} yield ()
/*
(defrel (do-ind-Nat t m b s o)
  (conde
   [(fresh (τ) (== t 'ZERO) (== b `(THE ,τ ,o)))]
   [(fresh (n res f^) (== t `(ADD1 ,n))
           (do-ind-Nat n m b s res)
           (do-appo s n f^)
           (do-appo f^ res o))]
   [(fresh (ne τ bas)
           (== t `(NEU NAT ,ne))
           (== o `(NEU ,τ (IND-NAT ,t ,m ,b ,s)))
           (do-appo m t τ))]))
*/
def doIndNat(t: VarOr[SExp], m: VarOr[SExp], b: VarOr[SExp], s: VarOr[SExp], o: VarOr[SExp]): Goal = conde(
  for {
    τ <- fresh[SExp]
    _ <- t === "ZERO"
    _ <- b === list("THE", τ, o)
  } yield (),
  for {
    n <- fresh[SExp]
    res <- fresh[SExp]
    f_ <- fresh[SExp]
    _ <- t === list("ADD1", n)
    _ <- doIndNat(n, m, b, s, res)
    _ <- doAppo(s, n, f_)
    _ <- doAppo(f_, res, o)
  } yield (),
  for {
    ne <- fresh[SExp]
    τ <- fresh[SExp]
    bas <- fresh[SExp]
    _ <- t === list("NEU", "NAT", ne)
    _ <- o === list("NEU", τ, list("IND-NAT", t, m, b, s))
    _ <- doAppo(m, t, τ)
  } yield ()
)
/*
(defrel (valof-= ρ exp v)
  (fresh (X from to Xv fromv tov)
    (== exp `(= ,X ,from ,to))
    (== v `(EQUAL ,Xv ,fromv ,tov))
    (valofo ρ X Xv)
    (valofo ρ from fromv)
    (valofo ρ to tov)))
*/
def valofEqual(ρ: VarOr[SExp], exp: VarOr[SExp], v: VarOr[SExp]): Goal = for {
  X <- fresh[SExp]
  from <- fresh[SExp]
  to <- fresh[SExp]
  Xv <- fresh[SExp]
  fromv <- fresh[SExp]
  tov <- fresh[SExp]
  _ <- exp === list("=", X, from, to)
  _ <- v === list("EQUAL", Xv, fromv, tov)
  _ <- valofo(ρ, X, Xv)
  _ <- valofo(ρ, from, fromv)
  _ <- valofo(ρ, to, tov)
} yield ()
/*
(defrel (valof-same ρ exp v)
  (fresh (e eᵥ)
    (== exp `(same ,e))
    (== v `(SAME ,eᵥ))
    (valofo ρ e eᵥ)))
*/
def valofSame(ρ: VarOr[SExp], exp: VarOr[SExp], v: VarOr[SExp]): Goal = for {
  e <- fresh[SExp]
  eV <- fresh[SExp]
  _ <- exp === list("same", e)
  _ <- v === list("SAME", eV)
  _ <- valofo(ρ, e, eV)
} yield ()
/*
(defrel (valof-ind-= ρ exp v)
  (fresh (t m b tV mV bV)
    (== exp `(ind-= ,t ,m ,b))
    (valofo ρ t tV)
    (valofo ρ m mV)
    (valofo ρ b bV)
    (do-ind-= ρ tV mV bV v)))
*/
def valofIndEqual(ρ: VarOr[SExp], exp: VarOr[SExp], v: VarOr[SExp]): Goal = for {
  t <- fresh[SExp]
  m <- fresh[SExp]
  b <- fresh[SExp]
  tV <- fresh[SExp]
  mV <- fresh[SExp]
  bV <- fresh[SExp]
  _ <- exp === list("ind-=", t, m, b)
  _ <- valofo(ρ, t, tV)
  _ <- valofo(ρ, m, mV)
  _ <- valofo(ρ, b, bV)
  _ <- doIndEqual(ρ, tV, mV, bV, v)
} yield ()
/*
(defrel (do-ind-= ρ t m b o)
  (conde
   [(fresh (v f1 τ) (== t  `(SAME ,v))
           (== o b))]
   [(fresh (A from to ne f1 τ vars Tvar p Ao Fo To f2 τb)
      (== t `(NEU (EQUAL ,A ,from ,to) ,ne))
      (== o `(NEU ,τ
                  (IND-=
                   (NEU (EQUAL ,A ,from ,to) ,ne)
                   (THE (PI ,Tvar ,A (CLOS ,ρ ,Tvar (Π ([,p (= ,Ao ,Fo ,To)]) U))) ,m)
                   (THE ,τb ,b))))(do-appo m to f1)
      (just-names ρ vars)
      (freshen 'to vars Tvar)
      (freshen 'p vars p)
      (do-appo f1 t τ)
      (read-back-typo ρ A Ao)
      (read-backo ρ A from Fo)
      (read-backo ρ A to To)
      (do-appo m from f2)
      (do-appo f2 `(SAME ,from) τb))]))
*/
def doIndEqual(ρ: VarOr[SExp], t: VarOr[SExp], m: VarOr[SExp], b: VarOr[SExp], o: VarOr[SExp]): Goal = conde(
  for {
    v <- fresh[SExp]
    f1 <- fresh[SExp]
    τ <- fresh[SExp]
    _ <- t === list("SAME", v)
    _ <- o === b
  } yield (),
  for {
    A <- fresh[SExp]
    from <- fresh[SExp]
    to <- fresh[SExp]
    ne <- fresh[SExp]
    f1 <- fresh[SExp]
    τ <- fresh[SExp]
    vars <- fresh[SExp]
    Tvar <- fresh[SExp]
    p <- fresh[SExp]
    Ao <- fresh[SExp]
    Fo <- fresh[SExp]
    To <- fresh[SExp]
    f2 <- fresh[SExp]
    τb <- fresh[SExp]
    _ <- t === list("NEU", list("EQUAL", A, from, to), ne)
    _ <- o === list("NEU", τ, list("IND-=", list("NEU", list("EQUAL", A, from, to), ne), list("THE", list("PI", Tvar, A, list("CLOS", ρ, Tvar, list("Π", list(list(p, list("=", Ao, Fo, To))), "U"))), m), list("THE", τb, b)))
    _ <- doAppo(m, to, f1)
    _ <- justNames(ρ, vars)
    _ <- freshen("to", vars, Tvar)
    _ <- freshen("p", vars, p)
    _ <- doAppo(f1, t, τ)
    _ <- readBackTypo(ρ, A, Ao)
    _ <- readBacko(ρ, A, from, Fo)
    _ <- readBacko(ρ, A, to, To)
    _ <- doAppo(m, from, f2)
    _ <- doAppo(f2, list("SAME", from), τb)
  } yield ()
)
// relevance functions for valofo
/*
(define (valofo-in exp)
  (match exp
    [(? simple?) (list exp)]
    [(? symbol?) '(var)]
    [(? (exp-memv? non-symbol-exprs)) (list (car exp))]
    [`(,rat ,ran) '(app)]
    [(? var?) '(use-maybe)]))
*/
def valofoIn(exp: VarOr[SExp])(walker: Walker): Seq[String] = walkStar(walker, exp) match {
  case exp: String if simpleQ(exp) => Seq(exp)
  case _: String => Seq("var")
  case exp if expMemvQ(nonSymbolExprs)(exp) => Seq(car(exp).asInstanceOf[String])
  case Cons(rat, ran) => Seq("app")
  case _: Var[_] => Seq(UseMaybe)
}
/*
(define (valofo-out v)
  (match v
    [`(NEU ,t ,e) '(var ind-Nat ind-= car cdr app)]
    [else all-exprs]))
*/
def valofoOut(v: VarOr[SExp])(walker: Walker): Seq[String] = walkStar(walker, v) match {
  case list("NEU", t, e) => Seq("var", "ind-Nat", "ind-=", "car", "cdr", "app")
  case _ => allExprs
}

/*

(defrel (valofo ρ exp v)
  (condp
    (((valofo-in exp))
     ((valofo-out v)))
    ; The expressions
    [the (valof-the ρ exp v)]
    [zero (assign-simple 'zero 'ZERO exp v)]
    [Atom (assign-simple 'Atom 'ATOM exp v)]
    [Nat (assign-simple 'Nat 'NAT exp v)]
    [U (assign-simple 'U 'UNIVERSE exp v)]
    [Trivial (assign-simple 'Trivial 'TRIVIAL exp v)]
    [sole (assign-simple 'sole 'SOLE exp v)]
    [var (apply-ρ ρ exp v)]
    [var (valof-neutral-var ρ exp v)]
    [quote (valof-quote ρ exp v)]
    [add1 (valof-add1 ρ exp v)]
    [ind-Nat (valof-ind-Nat ρ exp v)]
    [Σ (valof-Σ ρ exp v)]
    [cons (valof-cons ρ exp v)]
    [car (valof-car ρ exp v)]
    [cdr (valof-cdr ρ exp v)]
    [= (valof-= ρ exp v)]
    [same (valof-same ρ exp v)]
    [ind-= (valof-ind-= ρ exp v)]
    [Π (valof-Π ρ exp v)]
    [λ (valof-λ ρ exp v)]
    [app (valof-app ρ exp v)]))
*/
def valofo(ρ: VarOr[SExp], exp: VarOr[SExp], v: VarOr[SExp]): Goal = condp(Seq(valofoIn(exp)), Seq(valofoOut(v)))(
  ("the", valofThe(ρ, exp, v)),
  ("zero", assignSimple("zero", "ZERO", exp, v)),
  ("Atom", assignSimple("Atom", "ATOM", exp, v)),
  ("Nat", assignSimple("Nat", "NAT", exp, v)),
  ("U", assignSimple("U", "UNIVERSE", exp, v)),
  ("Trivial", assignSimple("Trivial", "TRIVIAL", exp, v)),
  ("sole", assignSimple("sole", "SOLE", exp, v)),
  ("var", applyρ(ρ, exp, v)),
  ("var", valofNeutralVar(ρ, exp, v)),
  ("quote", valofQuote(ρ, exp, v)),
  ("add1", valofAdd1(ρ, exp, v)),
  ("ind-Nat", valofIndNat(ρ, exp, v)),
  ("Σ", valofSigma(ρ, exp, v)),
  ("cons", valofCons(ρ, exp, v)),
  ("car", valofCar(ρ, exp, v)),
  ("cdr", valofCdr(ρ, exp, v)),
  ("=", valofEqual(ρ, exp, v)),
  ("same", valofSame(ρ, exp, v)),
  ("ind-=", valofIndEqual(ρ, exp, v)),
  ("Π", valofPi(ρ, exp, v)),
  ("λ", valofLam(ρ, exp, v)),
  ("app", valofApp(ρ, exp, v))
)
def valofo(ρ: VarOr[SExp], exp: VarOr[SExp]): Rel[SExp] = valofo(ρ, exp, _)
/*
(defrel (not-LAM e)
  (conde
    [(symbolo e)]
    [(fresh (a d)
       (== e `(,a . ,d))
       (=/= a 'LAM))]))
*/
def notLAM(e: VarOr[SExp]): Goal = conde(
  for {
    _ <- e.is[String]
  } yield (),
  for {
    a <- fresh[SExp]
    d <- fresh[SExp]
    _ <- e === list(a, d)
    _ <- a =/= "LAM"
  } yield ()
)
/*
;; helpers for read-backo
(defrel (read-back-λ Γ τ v norm)
  (fresh (x A c z x^ vars Γ^ B b inner Av)
    (== τ `(PI ,x ,A ,c))
    (== norm `(λ (,x^) ,inner))
    (conde
      [(fresh (y λc) (== v `(LAM ,y ,λc))
              (== z y))]
      [(not-LAM v) (== z x)])
    (symbolo z)
    (just-names Γ vars)
    (freshen z vars x^)
    (extend-Γ Γ x^ A Γ^)
    (valof-closuro c `(NEU ,A (VAR ,x^)) B)
    (do-appo v `(NEU ,A (VAR ,x^)) b)
    (read-backo Γ^ B b inner)))
*/
def readBackλ(Γ: VarOr[SExp], τ: VarOr[SExp], v: VarOr[SExp], norm: VarOr[SExp]): Goal =
  for {
    x <- fresh[SExp]
    A <- fresh[SExp]
    c <- fresh[SExp]
    z <- fresh[SExp]
    x_ <- fresh[SExp]
    vars <- fresh[SExp]
    Γ_ <- fresh[SExp]
    B <- fresh[SExp]
    b <- fresh[SExp]
    inner <- fresh[SExp]
    Av <- fresh[SExp]
    _ <- τ === list("PI", x, A, c)
    _ <- norm === list("λ", list(x_), inner)
    _ <- conde(
      for {
        y <- fresh[SExp]
        λc <- fresh[SExp]
        _ <- v === list("LAM", y, λc)
        _ <- z === y
      } yield (),
      for {
        _ <- notLAM(v)
        _ <- z === x
      } yield ()
    )
    _ <- z.is[String]
    _ <- justNames(Γ, vars)
    _ <- freshen(z, vars, x_)
    _ <- extendΓ(Γ, x_, A, Γ_)
    _ <- valofClosuro(c, list("NEU", A, list("VAR", x_)), B)
    _ <- doAppo(v, list("NEU", A, list("VAR", x_)), b)
    _ <- readBacko(Γ_, B, b, inner)
  } yield ()
/*
(defrel (read-back-same Γ τ v norm)
  (fresh (X from to val vo)
    (== τ `(EQUAL ,X ,from ,to))
    (== v `(SAME ,val))
    (== norm `(same ,vo))
    (read-backo Γ X val vo)))
*/
def readBackSame(Γ: VarOr[SExp], τ: VarOr[SExp], v: VarOr[SExp], norm: VarOr[SExp]): Goal =
  for {
    X <- fresh[SExp]
    from <- fresh[SExp]
    to <- fresh[SExp]
    value <- fresh[SExp]
    vo <- fresh[SExp]
    _ <- τ === list("EQUAL", X, from, to)
    _ <- v === list("SAME", value)
    _ <- norm === list("same", vo)
    _ <- readBacko(Γ, X, value, vo)
  } yield ()
/*
(defrel (read-back-cons Γ τ v norm)
  (fresh (x A c a a^ d d^ D)
    (== τ `(SIGMA ,x ,A ,c))
    (== norm `(cons ,a^ ,d^))
    (do-caro v a)
    (read-backo Γ A a a^)
    (valof-closuro c a D)
    (do-cdro v d)
    (read-backo Γ D d d^)))
*/
def readBackCons(Γ: VarOr[SExp], τ: VarOr[SExp], v: VarOr[SExp], norm: VarOr[SExp]): Goal =
  for {
    x <- fresh[SExp]
    A <- fresh[SExp]
    c <- fresh[SExp]
    a <- fresh[SExp]
    a_ <- fresh[SExp]
    d <- fresh[SExp]
    d_ <- fresh[SExp]
    D <- fresh[SExp]
    _ <- τ === list("SIGMA", x, A, c)
    _ <- norm === list("cons", a_, d_)
    _ <- doCaro(v, a)
    _ <- readBacko(Γ, A, a, a_)
    _ <- valofClosuro(c, a, D)
    _ <- doCdro(v, d)
    _ <- readBacko(Γ, D, d, d_)
  } yield ()
/*
(defrel (read-back-Nat Γ τ v norm)
  (== τ 'NAT)
  (conde
    [(== v 'ZERO) (== norm 'zero)]
    [(fresh (n nF)
       (== v `(ADD1 ,n))
       (== norm `(add1 ,nF))
       (read-backo Γ 'NAT n nF))]))
*/
def readBackNat(Γ: VarOr[SExp], τ: VarOr[SExp], v: VarOr[SExp], norm: VarOr[SExp]): Goal =
  for {
    n <- fresh[SExp]
    nF <- fresh[SExp]
    _ <- τ === "NAT"
    _ <- conde(
      for {
        _ <- v === "ZERO"
        _ <- norm === "zero"
      } yield (),
      for {
        _ <- v === list("ADD1", n)
        _ <- norm === list("add1", nF)
        _ <- readBacko(Γ, "NAT", n, nF)
      } yield ()
    )
  } yield ()
/*
(defrel (read-back-quote Γ τ v norm)
  (fresh (at)
    (== τ 'ATOM)
    (== v `(ATOM ,at))
    (symbolo at)
    (== norm `(quote ,at))))
*/
def readBackQuote(Γ: VarOr[SExp], τ: VarOr[SExp], v: VarOr[SExp], norm: VarOr[SExp]): Goal =
  for {
    at <- fresh[SExp]
    _ <- τ === "ATOM"
    _ <- v === list("ATOM", at)
    _ <- at.is[String]
    _ <- norm === list("quote", at)
  } yield ()
/*
(defrel (read-back-the Γ τ v norm)
  (fresh (t e tₒ eₒ)
    (== v `(THE ,t ,e))
    (== norm `(the ,tₒ ,eₒ))
    (read-back-typo Γ t tₒ)
    (read-backo Γ τ e eₒ)))
*/
def readBackThe(Γ: VarOr[SExp], τ: VarOr[SExp], v: VarOr[SExp], norm: VarOr[SExp]): Goal =
  for {
    t <- fresh[SExp]
    e <- fresh[SExp]
    t_ <- fresh[SExp]
    e_ <- fresh[SExp]
    _ <- v === list("THE", t, e)
    _ <- norm === list("the", t_, e_)
    _ <- readBackTypo(Γ, t, t_)
    _ <- readBacko(Γ, τ, e, e_)
  } yield ()
/*
(defrel (go-to-neutral Γ τ v norm)
  (fresh (τ ne)
    (== v `(NEU ,τ ,ne))
    (read-back-neutral τ Γ ne norm)))
*/
def goToNeutral(Γ: VarOr[SExp], τ: VarOr[SExp], v: VarOr[SExp], norm: VarOr[SExp]): Goal =
  for {
    τ_ <- fresh[SExp]
    ne <- fresh[SExp]
    _ <- v === list("NEU", τ_, ne)
    _ <- readBackNeutral(τ, Γ, ne, norm)
  } yield ()
/*
(defrel (go-to-type Γ τ v norm)
  (== τ 'UNIVERSE)
  (read-back-typo Γ v norm))
*/
def goToType(Γ: VarOr[SExp], τ: VarOr[SExp], v: VarOr[SExp], norm: VarOr[SExp]): Goal =
  for {
    _ <- τ === "UNIVERSE"
    _ <- readBackTypo(Γ, v, norm)
  } yield ()
/*

;; relevance function for read-backo

(define (in-type? e τ)
  (let ([cs (get-constructors τ)])
    (or (memv e cs)
        (and (pair? e)
             (member (car e) cs)))))
*/
def inType(e: VarOr[SExp], τ: VarOr[SExp]): Boolean =
  getConstructors(τ).contains(e) || (e match {
    case Cons(car, _) => getConstructors(τ).contains(car)
    case _ => false
  })
/*
(define (read-back-v v)
  (match v
    [`(THE ,t ,e) '(the)]
    [`(NEU ,t ,e) '(neutral)]
    [(? var?) '(use-maybe)]
    [else '()]))
*/
def readBackV(v: VarOr[SExp])(walker: Walker): Seq[String] = walkStar(walker, v) match {
  case list("THE", t, e) => Seq("the")
  case list("NEU", t, e) => Seq("neutral")
  case v: Var[_] => Seq(UseMaybe)
  case _ => Seq()
}
/*
(define (read-back-τ t)
  (match t
    ['UNIVERSE '(U)]
    ['TRIVIAL '(Trivial)]
    ['NAT '(Nat)]
    ['ATOM '(Atom)]
    [`(SIGMA . ,info) '(Σ)]
    [`(EQUAL . ,info) '(=)]
    [`(PI . ,info) '(Π)]
    [(? var?) '(use-maybe)]
    [else '(the neutral)]))
*/
def readBackT(t: VarOr[SExp])(walker: Walker): Seq[String] = walkStar(walker, t) match {
  case "UNIVERSE" => Seq("U")
  case "TRIVIAL" => Seq("Trivial")
  case "NAT" => Seq("Nat")
  case "ATOM" => Seq("Atom")
  case list("SIGMA", info@_*) => Seq("Σ")
  case list("EQUAL", info@_*) => Seq("=")
  case list("PI", info@_*) => Seq("Π")
  case v: Var[_] => Seq(UseMaybe)
  case _ => Seq("the", "neutral")
}
/*
(define (read-back-norm e)
  (let loop ([t '(Nat Trivial Atom Σ Π = U)])
    (cond
      [(null? t) '()]
      [(in-type? e (car t)) `(,(car t) neutral the)]
      [else (loop (cdr t))])))
*/
def readBackNorm(e0: VarOr[SExp])(walker: Walker): Seq[String] = {
  val e = walkStar(walker, e0)

  def loop(t: Seq[String] = Seq("Nat", "Trivial", "Atom", "Σ", "Π", "=", "U")): Seq[String] =
    if (t.isEmpty) Seq.empty
    else if inType(e, t.head) then Seq(t.head, "neutral", "the")
    else loop(t.tail)

  loop()
}
/*
(defrel (read-backo Γ τ v norm)
  (condp
    (((read-back-v v)
      (read-back-τ τ))
     ((read-back-norm norm)))
    ;; Types
    [U (go-to-type Γ τ v norm)]
    ;; The
    [the (read-back-the Γ τ v norm)]
    [neutral (go-to-neutral Γ τ v norm)]
    [Trivial (== τ 'TRIVIAL) (== v 'SOLE) (== norm 'sole)]
    [Atom (read-back-quote Γ τ v norm)]
    [Nat (read-back-Nat Γ τ v norm)]
    [Σ (read-back-cons Γ τ v norm)]
    [= (read-back-same Γ τ v norm)]
    [Π (read-back-λ Γ τ v norm)]))
*/
def readBacko(Γ: VarOr[SExp], τ: VarOr[SExp], v: VarOr[SExp], norm: VarOr[SExp]): Goal = condp(Seq(readBackV(v), readBackT(τ)), Seq(readBackNorm(norm)))(
  ("U", goToType(Γ, τ, v, norm)),
  ("the", readBackThe(Γ, τ, v, norm)),
  ("neutral", goToNeutral(Γ, τ, v, norm)),
  ("Trivial", begin(τ === "TRIVIAL", v === "SOLE", norm === "sole")),
  ("Atom", readBackQuote(Γ, τ, v, norm)),
  ("Nat", readBackNat(Γ, τ, v, norm)),
  ("Σ", readBackCons(Γ, τ, v, norm)),
  ("=", readBackSame(Γ, τ, v, norm)),
  ("Π", readBackλ(Γ, τ, v, norm))
)
def readBacko(Γ: VarOr[SExp], τ: VarOr[SExp], v: VarOr[SExp]): Rel[SExp] = readBacko(Γ, τ, v, _)
/*
(defrel (read-back-dep-binder tag₁ tag₂ Γ v norm)
  (fresh (x A c vars x^ A^ Dv D^ Γ^)
    (== v `(,tag₁ ,x ,A ,c))
    (== norm `(,tag₂ ([,x^ ,A^]) ,D^))
    (just-names Γ vars)
    (freshen x vars x^)
    (read-back-typo Γ A A^)
    (extend-Γ Γ x^ A Γ^)
    (valof-closuro c `(NEU ,A (VAR ,x^)) Dv)
    (read-back-typo Γ^ Dv D^)))
*/
def readBackDepBinder(tag1: String, tag2: String, Γ: VarOr[SExp], v: VarOr[SExp], norm: VarOr[SExp]): Goal = for {
  x <- fresh[SExp]
  A <- fresh[SExp]
  c <- fresh[SExp]
  vars <- fresh[SExp]
  x_ <- fresh[SExp]
  A_ <- fresh[SExp]
  Dv <- fresh[SExp]
  D_ <- fresh[SExp]
  Γ_ <- fresh[SExp]
  _ <- v === list(tag1, x, A, c)
  _ <- norm === list(tag2, list(list(x_, A_)), D_)
  _ <- justNames(Γ, vars)
  _ <- freshen(x, vars, x_)
  _ <- readBackTypo(Γ, A, A_)
  _ <- extendΓ(Γ, x_, A, Γ_)
  _ <- valofClosuro(c, list("NEU", A, list("VAR", x_)), Dv)
  _ <- readBackTypo(Γ_, Dv, D_)
} yield ()
/*
(defrel (read-back-= Γ v norm)
  (fresh (X to from Xo too fromo)
    (== v `(EQUAL ,X ,from ,to))
    (== norm `(= ,Xo ,fromo ,too))
    (read-back-typo Γ X Xo)
    (read-backo Γ X from fromo)
    (read-backo Γ X to too)))
*/
def readBackEqual(Γ: VarOr[SExp], v: VarOr[SExp], norm: VarOr[SExp]): Goal = for {
  X <- fresh[SExp]
  to <- fresh[SExp]
  from <- fresh[SExp]
  Xo <- fresh[SExp]
  too <- fresh[SExp]
  fromo <- fresh[SExp]
  _ <- v === list("EQUAL", X, from, to)
  _ <- norm === list("=", Xo, fromo, too)
  _ <- readBackTypo(Γ, X, Xo)
  _ <- readBacko(Γ, X, from, fromo)
  _ <- readBacko(Γ, X, to, too)
} yield ()
/*
(defrel (read-back-type-neutral Γ v norm)
  (fresh (ne)
    (== v `(NEU UNIVERSE ,ne))
    (read-back-neutral 'UNIVERSE Γ ne norm)))
*/
def readBackTypeNeutral(Γ: VarOr[SExp], v: VarOr[SExp], norm: VarOr[SExp]): Goal = for {
  ne <- fresh[SExp]
  _ <- v === list("NEU", "UNIVERSE", ne)
  _ <- readBackNeutral("UNIVERSE", Γ, ne, norm)
} yield ()
/*
(define (RBT-v v)
  (match v
    [(? symbol?) `(,v)]
    [`(PI . ,info) '(Π)]
    [`(EQUAL . ,info) '(=)]
    [`(SIGMA . ,info) '(Σ)]
    [`(NEU . ,info) '(neutral)]
    [(? var?) '(use-maybe)]
    [else '()]))
*/
def RBTv(v: VarOr[SExp])(walker: Walker): Seq[String] = walkStar(walker, v) match {
  case v: String => Seq(v)
  case Cons("PI", info) => Seq("Π")
  case Cons("EQUAL", info) => Seq("=")
  case Cons("SIGMA", info) => Seq("Σ")
  case Cons("NEU", info) => Seq("neutral")
  case v: Var[_] => Seq(UseMaybe)
  case _ => Seq()
}
/*
(define (RBT-n e)
  (match e
    ['Atom '(ATOM)]
    ['Trivial '(TRIVIAL)]
    ['Nat '(NAT)]
    ['U '(UNIVERSE)]
    [`(Π . ,info) '(Π)]
    [`(= . ,info) '(=)]
    [`(Σ . ,info) '(Σ)]
    [(? var?) '(ATOM NAT UNIVERSE TRIVIAL Σ Π = neutral)]
    [else '(neutral)]))
*/
def RBTn(e: VarOr[SExp])(walker: Walker): Seq[String] = walkStar(walker, e) match {
  case "Atom" => Seq("ATOM")
  case "Trivial" => Seq("TRIVIAL")
  case "Nat" => Seq("NAT")
  case "U" => Seq("UNIVERSE")
  case Cons("Π", info) => Seq("Π")
  case Cons("=", info) => Seq("=")
  case Cons("Σ", info) => Seq("Σ")
  case e: Var[_] => Seq("ATOM", "NAT", "UNIVERSE", "TRIVIAL", "Σ", "Π", "=", "neutral")
  case _ => Seq("neutral")
}
/*
(defrel (read-back-typo Γ v norm)
  (condp
    (((RBT-v v))
     ((RBT-n norm)))
    [ATOM (assign-simple 'ATOM 'Atom v norm)]
    [NAT (assign-simple 'NAT 'Nat v norm)]
    [UNIVERSE (assign-simple 'UNIVERSE 'U v norm)]
    [TRIVIAL (assign-simple 'TRIVIAL 'Trivial v norm)]
    [Σ (read-back-dep-binder 'SIGMA 'Σ Γ v norm)]
    [= (read-back-= Γ v norm)]
    [Π (read-back-dep-binder 'PI 'Π Γ v norm)]
    [neutral (read-back-type-neutral Γ v norm)]))
*/
def readBackTypo(Γ: VarOr[SExp], v: VarOr[SExp], norm: VarOr[SExp]): Goal = condp(Seq(RBTv(v)),Seq(RBTn(norm)))(
  ("ATOM", assignSimple("ATOM", "Atom", v, norm)),
  ("NAT", assignSimple("NAT", "Nat", v, norm)),
  ("UNIVERSE", assignSimple("UNIVERSE", "U", v, norm)),
  ("TRIVIAL", assignSimple("TRIVIAL", "Trivial", v, norm)),
  ("Σ", readBackDepBinder("SIGMA", "Σ", Γ, v, norm)),
  ("=", readBackEqual(Γ, v, norm)),
  ("Π", readBackDepBinder("PI", "Π", Γ, v, norm)),
  ("neutral", readBackTypeNeutral(Γ, v, norm)))
/*
(defrel (RBN-var ne norm)
  (fresh (s)
    (== ne `(VAR ,s))
    (== norm s)))
*/
def RBNvar(ne: VarOr[SExp], norm: VarOr[SExp]): Goal = for {
  s <- fresh[SExp]
  _ <- ne === list("VAR", s)
  _ <- norm === s
} yield ()
/*
(defrel (RBN-car τ Γ ne norm)
  (fresh (pr τ-pr pr^)
    (== ne `(CAR (NEU ,τ-pr ,pr)))
    (== norm `(car ,pr^))
    (read-back-neutral τ-pr Γ pr pr^)))
*/
def RBNcar(τ: VarOr[SExp], Γ: VarOr[SExp], ne: VarOr[SExp], norm: VarOr[SExp]): Goal = for {
  pr <- fresh[SExp]
  τpr <- fresh[SExp]
  pr_ <- fresh[SExp]
  _ <- ne === list("CAR", list("NEU", τpr, pr))
  _ <- norm === list("car", pr_)
  _ <- readBackNeutral(τpr, Γ, pr, pr_)
} yield ()
/*
(defrel (RBN-cdr τ Γ ne norm)
  (fresh (τ-pr pr pr^)
    (== ne `(CDR (NEU ,τ-pr ,pr)))
    (== norm `(cdr ,pr^))
    (read-back-neutral τ-pr Γ pr pr^)))
*/
def RBNcdr(τ: VarOr[SExp], Γ: VarOr[SExp], ne: VarOr[SExp], norm: VarOr[SExp]): Goal = for {
  τpr <- fresh[SExp]
  pr <- fresh[SExp]
  pr_ <- fresh[SExp]
  _ <- ne === list("CDR", list("NEU", τpr, pr))
  _ <- norm === list("cdr", pr_)
  _ <- readBackNeutral(τpr, Γ, pr, pr_)
} yield ()
/*
(defrel (RBN-app τ Γ ne norm)
  (fresh (rat ran rato rano x A c T)
    (== ne `(N-APP (NEU (PI ,x ,A ,c) ,rat) ,ran))
    (== norm `(the ,T (,rato ,rano)))
    (read-back-neutral `(PI ,x ,A ,c) Γ rat rato)
    (read-backo Γ A ran rano)
    (read-back-typo Γ τ T)))
*/
def RBNapp(τ: VarOr[SExp], Γ: VarOr[SExp], ne: VarOr[SExp], norm: VarOr[SExp]): Goal = for {
  rat <- fresh[SExp]
  ran <- fresh[SExp]
  rato <- fresh[SExp]
  rano <- fresh[SExp]
  x <- fresh[SExp]
  A <- fresh[SExp]
  c <- fresh[SExp]
  T <- fresh[SExp]
  _ <- ne === list("N-APP", list("NEU", list("PI", x, A, c), rat), ran)
  _ <- norm === list("the", T, list(rato, rano))
  _ <- readBackNeutral(list("PI", x, A, c), Γ, rat, rato)
  _ <- readBacko(Γ, A, ran, rano)
  _ <- readBackTypo(Γ, τ, T)
} yield ()
/*
(defrel (RBN-ind-Nat τ Γ ne norm)
  (fresh (t m b s to mo bo so T τB TB T1 T2 vars n-1 res Γ^ k-1)
    (== ne `(IND-NAT (NEU NAT ,t) ,m (THE ,τB ,b) ,s))
    (== norm `(ind-Nat ,to ,mo (the ,TB ,bo) ,so))
    (read-back-neutral 'NAT Γ t to)
    (just-names Γ vars)
    (freshen 'k-1 vars k-1)
    (read-backo Γ `(PI ,k-1 NAT (CLOS ,Γ ,k-1 U)) m mo)
    (read-backo Γ τB b bo)
    (read-back-typo Γ τ T)
    (read-back-typo Γ τB TB)
    (freshen 'n-1 vars n-1)
    (freshen 'res vars res)
    (extend-Γ Γ n-1 'NAT Γ^)
    (read-backo Γ `(PI ,n-1 NAT (CLOS ,Γ ,n-1 (Π ([,res (,mo ,n-1)])
                                                (,mo (add1 ,n-1))))) s so)))
*/
def RBNindNat(τ: VarOr[SExp], Γ: VarOr[SExp], ne: VarOr[SExp], norm: VarOr[SExp]): Goal = for {
  t <- fresh[SExp]
  m <- fresh[SExp]
  b <- fresh[SExp]
  s <- fresh[SExp]
  to <- fresh[SExp]
  mo <- fresh[SExp]
  bo <- fresh[SExp]
  so <- fresh[SExp]
  T <- fresh[SExp]
  τB <- fresh[SExp]
  TB <- fresh[SExp]
  T1 <- fresh[SExp]
  T2 <- fresh[SExp]
  vars <- fresh[SExp]
  n_1 <- fresh[SExp]
  res <- fresh[SExp]
  Γ_ <- fresh[SExp]
  k_1 <- fresh[SExp]
  _ <- ne === list("IND-NAT", list("NEU", "NAT", t), m, list("THE", τB, b), s)
  _ <- norm === list("ind-Nat", to, mo, list("the", TB, bo), so)
  _ <- readBackNeutral("NAT", Γ, t, to)
  _ <- justNames(Γ, vars)
  _ <- freshen("k-1", vars, k_1)
  _ <- readBacko(Γ, list("PI", k_1, "NAT", list("CLOS", Γ, k_1, "U")), m, mo)
  _ <- readBacko(Γ, τB, b, bo)
  _ <- readBackTypo(Γ, τ, T)
  _ <- readBackTypo(Γ, τB, TB)
  _ <- freshen("n-1", vars, n_1)
  _ <- freshen("res", vars, res)
  _ <- extendΓ(Γ, n_1, "NAT", Γ_)
  _ <- readBacko(Γ, list("PI", n_1, "NAT", list("CLOS", Γ, n_1, list("Π", list(list(res, list(mo, n_1))), list(mo, list("add1", n_1))))), s, so)
} yield ()
/*
(defrel (RBN-ind-= τ Γ ne norm)
  (fresh (A from to ne1 τm m τb b neo mo bo)
    (== ne `(IND-= (NEU (EQUAL ,A ,from ,to) ,ne1)
                   (THE ,τm ,m)
                   (THE ,τb ,b)))
    (== norm `(ind-= ,neo ,mo ,bo))
    (read-back-neutral `(EQUAL ,A ,from ,to) Γ ne1 neo)
    (read-backo Γ τm m mo)
    (read-backo Γ τb b bo)))
*/
def RBNindEq(τ: VarOr[SExp], Γ: VarOr[SExp], ne: VarOr[SExp], norm: VarOr[SExp]): Goal = for {
  A <- fresh[SExp]
  from <- fresh[SExp]
  to <- fresh[SExp]
  ne1 <- fresh[SExp]
  τm <- fresh[SExp]
  m <- fresh[SExp]
  τb <- fresh[SExp]
  b <- fresh[SExp]
  neo <- fresh[SExp]
  mo <- fresh[SExp]
  bo <- fresh[SExp]
  _ <- ne === list("IND-=", list("NEU", list("EQUAL", A, from, to), ne1), list("THE", τm, m), list("THE", τb, b))
  _ <- norm === list("ind-=", neo, mo, bo)
  _ <- readBackNeutral(list("EQUAL", A, from, to), Γ, ne1, neo)
  _ <- readBacko(Γ, τm, m, mo)
  _ <- readBacko(Γ, τb, b, bo)
} yield ()
/*
;; relevance function for read-back-neutral

(define all-RBN
  '(VAR CAR CDR N-APP IND-NAT IND-=))
*/
val allRBN: Seq[String] = Seq("VAR", "CAR", "CDR", "N-APP", "IND-NAT", "IND-=")
/*
(define (RBN-ne v)
  (match v
    [(? (exp-memv? all-RBN)) `(,(car v))]
    [(? var?) all-RBN]
    [else '()]))
*/
def RBNne(v0: VarOr[SExp])(walker: Walker): Seq[String] = {
  val v = walkStar(walker, v0)
  if(expMemvQ(allRBN)(v)) Seq(v.asInstanceOf[Cons].a.asInstanceOf[String])
  else if(v.isInstanceOf[Var[_]]) allRBN
  else Seq()
}
def readBackNeutral(τ: VarOr[SExp], Γ: VarOr[SExp], ne: VarOr[SExp], norm: VarOr[SExp]): Goal = ???