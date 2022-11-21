package littlejian.examples.evalo

import littlejian._
import littlejian.ext._
import littlejian.data.sexp._


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

// checkido: check in definition
// notInEnvo: check in usage
// choose one

//def checkido(x: VarOr[SExp]): Goal = x.isType[String] && x =/= "lambda" && x =/= "quote" && x =/= "cons" && x =/= "list" && x =/= "car" && x =/= "cdr" && x =/= ClosureTag

def notInEnvo(env: VarOr[SExp], id: VarOr[SExp]): Goal = conde(
  env === (),
  for {
    (aid, av, d) <- fresh[SExp, SExp, SExp]
    _ <- env === cons(cons(aid, av), d) && id =/= aid && notInEnvo(d, id)
  } yield ()
)

def envExto(env: VarOr[SExp], params: VarOr[SExp], args: VarOr[SExp]): Rel[SExp] = conde(
  begin(params === (), args === (), env),
  for {
    (arg, args0) <- args.is(cons)
    (param, params0) <- params.is(cons)
    //_ <- checkido(param)
    newEnv <- envExto(cons(cons(param, arg), env), params0, args0)
  } yield newEnv
)

val ClosureTag = "$$Closure$$"

def closureo(env: VarOr[SExp], params: VarOr[SExp], body: VarOr[SExp]): VarOr[SExp] = cons(ClosureTag, cons(env, cons(params, body)))

def applyo(f: VarOr[SExp], args: VarOr[SExp]): Rel[SExp] =
  for {
    (env, params, body) <- f.is[SExp, SExp, SExp](closureo)
    result <- evalo app(envExto(env, params, args), body)
  } yield result

def applyo(f: VarOr[SExp], args: VarOr[SExp], result: VarOr[SExp]): Goal = applyo(f, args)(result)

def evalo(env: VarOr[SExp], x: VarOr[SExp], result: VarOr[SExp]): Goal = evalo(env, x)(result)

def evalo(env: VarOr[SExp], x: VarOr[SExp]): Rel[SExp] = conde(
  x.isType[String] >> lookupo(env, x),
  for {
    (f, args) <- x.is(cons)
    result <- applyo app(evalo(env, f), mapo(evalo(env, _), args))
  } yield result,
  for {
    _ <- notInEnvo(env, "lambda")
    (args, body) <- x.is[SExp, SExp](list("lambda", _, _))
  } yield closureo(env, args, body),
  for {
    ignored <- notInEnvo(env, "quote")
    result <- x.is[SExp](list("quote", _))
    //ignored <- result.isType[String] // a hack to prevent run[SExp] { x => evalo((), x, x) }.head => "(quote #1=(quote #1))"
    ignored <- result.absent(ClosureTag)
  } yield result,
  for {
    ignored <- notInEnvo(env, "cons")
    (a, b) <- x.is[SExp, SExp](list("cons", _, _))
    result <- cons call(evalo(env, a), evalo(env, b))
  } yield result,
  for {
    _ <- notInEnvo(env, "list")
    xs <- x.is[SExp](cons("list", _))
    result <- mapo(evalo(env, _), xs)
  } yield result,
  for {
    _ <- notInEnvo(env, "car")
    p <- x.is[SExp](list("car", _))
    p0 <- evalo(env, p)
    (a, b) <- p0.is[SExp, SExp](cons)
    _ <- a =/= ClosureTag
  } yield a,
  for {
    _ <- notInEnvo(env, "cdr")
    p <- x.is[SExp](list("cdr", _))
    p0 <- evalo(env, p)
    (a, b) <- p0.is[SExp, SExp](cons)
    _ <- a =/= ClosureTag
  } yield b,
)