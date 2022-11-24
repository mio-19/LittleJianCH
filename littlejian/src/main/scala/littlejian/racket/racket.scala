package littlejian.racket

import littlejian.*
import littlejian.data.sexp.*

import scala.annotation.tailrec
import scala.collection.mutable

type SExpr = VarOr[SExp]

final class Local(val vars: mutable.HashMap[String, SExpr] = new mutable.HashMap(), val macros: mutable.HashMap[String, SExpLambda] = new mutable.HashMap()) {

}

final case class Env(stack: List[Local] = List(new Local())) {
  def child: Env = Env(new Local() :: stack)

  def lookup(x: String): Option[SExpr] = {
    if (lookupMacro(x).isDefined) throw new IllegalStateException("macro name conflict: " + x)
    var s = stack
    while (s.nonEmpty) {
      val head = s.head
      val tail = s.tail
      head.vars.get(x) match {
        case Some(r) => return Some(r)
        case None => {
          s = tail
        }
      }
    }
    None
  }

  def lookupMacro(x: String): Option[SExpLambda] = {
    var s = stack
    while (s.nonEmpty) {
      val head = s.head
      val tail = s.tail
      head.macros.get(x) match {
        case Some(r) => return Some(r)
        case None => {
          s = tail
        }
      }
    }
    None
  }

  def update(key: String, value: SExpr): Unit = stack.head.vars.update(key, value)

  def updateMacro(key: String, value: SExpLambda): Unit = stack.head.macros.update(key, value)
}

val globalEnv: Env = {
  val globalEnv = Env()
  globalEnv.update("cons", Cons(_, _))
  globalEnv.update("car", sExpLambda1 {
    case Cons(a, _) => a
  })
  globalEnv.update("cdr", sExpLambda1 {
    case Cons(_, b) => b
  })
  globalEnv.update("pair?", sExpLambda1 {
    case Cons(_, _) => true
    case _ => false
  })
  globalEnv.update("null?", sExpLambda1 {
    case () => true
    case _ => false
  })
  globalEnv.update("append", sExpLambda { xs =>
    xs.fold(())(append)
  })
  globalEnv.update("list", sExpLambda { xs =>
    list(xs*)
  })
  globalEnv.update("list?", sExpLambda1 {
    case list(_*) => true
    case _ => false
  })
  globalEnv.update("apply", sExpLambda2 {
    case (f: SExpLambda, list(args*)) => f(args)
    case _ => throw new IllegalArgumentException("apply: invalid arguments")
  })
  globalEnv.update("vector", sExpLambda { xs =>
    Vector(xs*)
  })
  globalEnv.update("vector?", sExpLambda1 {
    case _: SExpVector => true
    case _ => false
  })
  globalEnv
}





object ARGS {
  def unapply(x: SExpr): Option[(List[String], Option[String])] = x match {
    case rest: String => Some((Nil, Some(rest)))
    case Cons(head: String, rest) => unapply(rest).map { case (xs, y) => (head +: xs, y) }
    case () => Some((Nil, None))
    case _ => None
  }
}

def parseArgs(xs: List[String], rest: Option[String], args: Seq[SExpr], env: Env): Unit = (xs, rest) match {
  case (Nil, Some(id)) => env.update(id, list(args *))
  case (Nil, None) if args.isEmpty => {}
  case (x :: xs, rest) if args.nonEmpty => {
    env.update(x, args.head)
    parseArgs(xs, rest, args.tail, env)
  }
  case _ => throw new IllegalArgumentException("Invalid arguments")
}

def eval(exp: SExpr): SExpr = eval(globalEnv, exp)
def eval(env: Env, exp: SExpr): SExpr = exp match {
  case list("define", name: String, body) => {
    env.update(name, eval(env, body))
    ()
  }
  case list("define", Cons(name, args), body*) => eval(env, list("define", name, append(list("lambda", args), list(body *))))
  case "define" => throw new IllegalStateException("Invalid define")
  case list("define-macro", name: String, body) => {
    env.updateMacro(name, eval(env, body).asInstanceOf[SExpLambda])
    ()
  }
  case list("define-macro", Cons(name, args), body*) => eval(env, list("define-macro", name, append(list("lambda", args), list(body *))))
  case "define-macro" => throw new IllegalStateException("Invalid define-macro")
  case list("begin", xs*) => evalBegin(env, xs)
  case "begin" => throw new IllegalStateException("Invalid begin")
  case list("lambda", args, body*) => args match {
    case ARGS(xs, rest) => SExpLambda(argVec => {
      val env0 = env.child
      parseArgs(xs, rest, argVec, env0)
      evalBegin(env0, body)
    })
    case _ => throw new IllegalArgumentException("Invalid arguments pattern")
  }
  case "lambda" => throw new IllegalStateException("Invalid lambda")
  case list("quote", x) => x
  case "quote" => throw new IllegalStateException("Invalid quote")
  case list("quasiquote", x) => quasiquote(env, x)
  case "quasiquote" => throw new IllegalStateException("Invalid quasiquote")
  case "unquote" => throw new IllegalStateException("Invalid unquote")
  case "unquote-splicing" => throw new IllegalStateException("Invalid unquote-splicing")
  case v: String => env.lookup(v).get
  case list(f, xs*) => {
    if (f.isInstanceOf[String]) {
      val maybeMacro = env.lookupMacro(f.asInstanceOf[String])
      if (maybeMacro.isDefined) return eval(env, maybeMacro.get.apply(xs))
    }
    eval(env, f).asInstanceOf[SExpLambda].apply(xs.map(eval(env, _)))
  }
}

def quasiquote(env: Env, x: SExpr): SExpr = x match {
  case list("unquote", x) => eval(env, x)
  case "unquote" => throw new IllegalStateException("Invalid unquote")
  case "unquote-splicing" => throw new IllegalStateException("Invalid unquote-splicing")
  case Cons(list("unquote-splicing", xs), x) => append(eval(env, xs), quasiquote(env, x))
  case Cons(x, list("unquote-splicing", xs)) => Cons(x, eval(env, xs))
  case Cons(x, y) => Cons(quasiquote(env, x), quasiquote(env, y))
  case a => a
}

def append(xs: SExpr, ys: SExpr): SExpr = xs match {
  case Cons(x, xs) => Cons(x, append(xs, ys))
  case () => ys
  case _ => throw new IllegalStateException("Invalid append")
}

@tailrec
def evalBegin(env: Env, xs: Seq[SExpr]): SExpr = xs.toList match {
  case Nil => ()
  case x :: Nil => eval(env, x)
  case x :: xs => {
    eval(env, x)
    evalBegin(env, xs)
  }
}