package littlejian.examples.evalo

import littlejian._
import littlejian.ext._
import littlejian.data.sexp._

def lookupo(env: VarOr[SExp], id: VarOr[SExp], v: VarOr[SExp]): Goal = conde(
  {
    val d = hole[SExp]
    id.isType[String] && env === cons(cons(id, v), d)
  },
  {
    val aid = hole[SExp]
    val av = hole[SExp]
    val d = hole[SExp]
    id.isType[String] && env === cons(cons(aid, av), d) && id =/= aid && lookupo(d, id, v)
  }
)

def lookupo(env: VarOr[SExp], id: VarOr[SExp]): Rel[SExp] = lookupo(env, id, _)

// checkido: check in definition
// notInEnvo: check in usage
// choose one

//def checkido(x: VarOr[SExp]): Goal = x.isType[String] && x =/= "lambda" && x =/= "quote" && x =/= "cons" && x =/= "list" && x =/= "car" && x =/= "cdr" && x =/= ClosureTag

def notInEnvo(env: VarOr[SExp], id: VarOr[SExp]): Goal = conde(
  env === (),
  {
    val aid = hole[SExp]
    val av = hole[SExp]
    val d = hole[SExp]
    env === cons(cons(aid, av), d) && id =/= aid && notInEnvo(d, id)
  }
)

def envExto(env: VarOr[SExp], params: VarOr[SExp], args: VarOr[SExp]): Rel[SExp] = conde(
  begin(params === (), args === (), env),
  {
    val arg = hole[SExp]
    val args0 = hole[SExp]
    val param = hole[SExp]
    val params0 = hole[SExp]
    for {
      _ <- args === cons(arg, args0)
      _ <- params === cons(param, params0)
      //_ <- checkido(param)
      newEnv <- envExto(cons(cons(param, arg), env), params0, args0)
    } yield newEnv
  }
)

val ClosureTag = "$$Closure$$"

def closureo(env: VarOr[SExp], params: VarOr[SExp], body: VarOr[SExp]): VarOr[SExp] = cons(ClosureTag, cons(env, cons(params, body)))

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

def mapo(f: VarOr[SExp] => Rel[SExp], xs: VarOr[SExp]): Rel[SExp] = conde(
  begin(xs === (), ()),
  {
    val head = hole[SExp]
    val tail = hole[SExp]
    for {
      _ <- xs === cons(head, tail)
      head0 <- f(head)
      tail0 <- mapo(f, tail)
    } yield cons(head0, tail0)
  }
)

def evalo(env: VarOr[SExp], x: VarOr[SExp], result: VarOr[SExp]): Goal = evalo(env, x)(result)

def evalo(env: VarOr[SExp], x: VarOr[SExp]): Rel[SExp] = conde(
  lookupo(env, x),
  {
    val f = hole[SExp]
    val args = hole[SExp]
    for {
      _ <- x === cons(f, args)
      f0 <- evalo(env, f)
      args0 <- mapo(evalo(env, _), args)
      result <- applyo(f0, args0)
    } yield result
  },
  {
    val args = hole[SExp]
    val body = hole[SExp]
    for {
      _ <- notInEnvo(env, "lambda")
      _ <- x === list("lambda", args, body)
    } yield closureo(env, args, body)
  },
  {
    val result = hole[SExp]
    for {
      _ <- notInEnvo(env, "quote")
      //_ <- result.isType[String] // a hack to prevent run[SExp] { x => evalo((), x, x) }.head => "(quote #1=(quote #1))"
      _ <- result.absent(ClosureTag)
      _ <- x === list("quote", result)
    } yield result
  },
  {
    val a = hole[SExp]
    val b = hole[SExp]
    for {
      _ <- notInEnvo(env, "cons")
      _ <- x === list("cons", a, b)
      a0 <- evalo(env, a)
      b0 <- evalo(env, b)
    } yield cons(a0, b0)
  },
  {
    val xs = hole[SExp]
    for {
      _ <- notInEnvo(env, "list")
      _ <- x === cons("list", xs)
      xs0 <- mapo(evalo(env, _), xs)
    } yield xs0
  },
  {
    val p = hole[SExp]
    val a = hole[SExp]
    val b = hole[SExp]
    for {
      _ <- notInEnvo(env, "car")
      _ <- x === list("car", p)
      p0 <- evalo(env, p)
      _ <- p0 === cons(a, b)
      _ <- a =/= ClosureTag
    } yield a
  },
  {
    val p = hole[SExp]
    val a = hole[SExp]
    val b = hole[SExp]
    for {
      _ <- notInEnvo(env, "cdr")
      _ <- x === list("cdr", p)
      p0 <- evalo(env, p)
      _ <- p0 === cons(a, b)
      _ <- a =/= ClosureTag
    } yield b
  }
)