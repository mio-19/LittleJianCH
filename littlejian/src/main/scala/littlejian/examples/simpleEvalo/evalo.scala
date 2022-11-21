package littlejian.examples.simpleEvalo

import littlejian._
import littlejian.ext._
import littlejian.data.sexp._
import littlejian.data._

// translated from section 4 of "miniKanren, Live and Untagged Quine Generation via Relational Interpreters (Programming Pearl)"

def lookupo(env: VarOr[SExp], id: VarOr[SExp], v: VarOr[SExp]): Goal = conde(
  for {
    d <- fresh[SExp]
    _ <- env === cons(cons(id, v), d)
  } yield (),
  for {
    (aid, av, d) <- fresh[SExp, SExp, SExp]
    _ <- env === cons(cons(aid, av), d) && id =/= aid && lookupo(d, id, v)
  } yield ()
)


def lookupo(env: VarOr[SExp], id: VarOr[SExp]): Rel[SExp] = lookupo(env, id, _)

def notInEnvo(env: VarOr[SExp], id: VarOr[SExp]): Goal = conde(
  env === (),
  for {
    (aid, av, d) <- fresh[SExp, SExp, SExp]
    _ <- env === cons(cons(aid, av), d) && id =/= aid && notInEnvo(d, id)
  } yield ()
)

def evalExpo(exp: VarOr[SExp], env: VarOr[SExp], o: VarOr[SExp]): Goal = evalExpo(exp, env)(o)
def evalExpo(exp: VarOr[SExp], env: VarOr[SExp]): Rel[SExp] = conde(
  for {
    v <- exp.is[SExp](list("quote", _))
    _ <- notInEnvo(env, "quote")
    _ <- v.absent(Str("closure"))
  } yield v,
  for {
    as <- exp.is[SExp](cons("list", _))
    _ <- notInEnvo(env, "list")
    result <- mapo(evalExpo(_, env), as)
  } yield result,
  exp.isType[Str] >> lookupo(env, exp),
  for {
    (rator, rand) <- exp.is(cons)
    f <- evalExpo(rator, env)
    (x, body, env0) <- f.is[SExp, SExp, SExp](list("closure", _, _, _))
    a <- evalExpo(rand, env)
    result <- evalExpo(body, cons(cons(x, a), env0))
  } yield result,
  for {
    (x, body) <- exp.is[SExp, SExp]((x, body) => list("lambda", list(x), body))
    _ <- x.isType[Str]
    _ <- notInEnvo(env, "lambda")
  } yield list("closure", x, body, env)
)