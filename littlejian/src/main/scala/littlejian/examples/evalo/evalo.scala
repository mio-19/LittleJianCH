package littlejian.examples.evalo

import littlejian._
import littlejian.ext._
import littlejian.data.sexp._

def lookupo(env: VarOr[SExp], id: VarOr[SExp], v: VarOr[SExp]): Goal = conde(
  {
    val d = hole[SExp]
    env === cons(cons(id, v), d)
  },
  {
    val aid = hole[SExp]
    val av = hole[SExp]
    val d = hole[SExp]
    env === cons(cons(aid, av), d) && id =/= aid && lookupo(d, id, v)
  }
)

def lookupo(env: VarOr[SExp], id: VarOr[SExp]): Rel[SExp] = lookupo(env, id, _)

def envExto(env: VarOr[SExp], params: VarOr[SExp], args: VarOr[SExp]): Rel[SExp] = conde(
  begin(params === (), args === (), env),
  {
    val arg = hole[SExp]
    val args0 = hole[SExp]
    val param = hole[SExp]
    val params0 = hole[SExp]
    begin(
      args === cons(arg, args0),
      params === cons(param, params0),
      cons(cons(param, arg), env))
  }
)

def closureo(env: VarOr[SExp], params: VarOr[SExp], body: VarOr[SExp]): VarOr[SExp] = cons("closure", cons(env, cons(params, body)))

def applyo(f: VarOr[SExp], args: VarOr[SExp]): Rel[SExp] = {
  val env = hole[SExp]
  val params = hole[SExp]
  val body = hole[SExp]
  for {
    _ <- f === closureo(env, params, body)
    newEnv <- envExto(env, params, args)
    result <- evalo(newEnv, body)
  } yield result
}

def applyo(f: VarOr[SExp], args: VarOr[SExp], result: VarOr[SExp]): Goal = applyo(f, args)(result)

def evalo(env: VarOr[SExp], x: VarOr[SExp]): Rel[SExp] = ???