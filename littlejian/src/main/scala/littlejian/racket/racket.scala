package littlejian.racket

import littlejian._
import littlejian.data.sexp.*

import scala.collection.mutable

type SExpr = VarOr[SExp]

final case class Env(stack: List[mutable.HashMap[String, SExpr]]) {
  def child: Env = Env(new mutable.HashMap[String, SExpr]() :: stack)

  def lookup(x: String): Option[SExpr] = {
    var s = stack
    while (s.nonEmpty) {
      val head = s.head
      val tail = s.tail
      head.get(x) match {
        case Some(r) => return Some(r)
        case None => {
          s = tail
        }
      }
    }
    None
  }

  def update(key: String, value: SExpr): Unit = stack.head.update(key, value)
}

object LIST {
  def apply(xs: SExpr*) = list(xs *)

  def unapplySeq(x: SExpr): Option[Seq[SExpr]] = x match {
    case () => Some(Seq())
    case Cons(x, xs) => unapplySeq(xs).map(x +: _)
    case _ => None
  }
}

object LISTrest {
  def unapply(x: SExpr): Option[(List[SExpr], SExpr)] = x match {
    case Cons(x, xs) => unapply(xs).map { case (ys, z) => (x +: ys, z) }
    case other => Some((Nil, other))
  }
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
  case (Nil, None) if args.isEmpty => env
  case (x :: xs, rest) if args.nonEmpty => {
    env.update(x, args.head)
    parseArgs(xs, rest, args.tail, env)
  }
  case _ => throw new IllegalArgumentException("Invalid arguments")
}

def eval(env: Env, exp: SExpr): SExpr = exp match {
  case list("define", name: String, body: SExp) => {
    env.update(name, eval(env, body))
    ()
  }
  case LIST("begin", xs@_*) => evalBegin(env, xs.toList)
  case LIST("lambda", args, body*) => args match {
    case ARGS(xs, rest) => SExpLambda(argVec => {
      val env0 = env.child
      parseArgs(xs, rest, argVec, env0)
    })
  }
}

def evalBegin(env: Env, xs: List[SExpr]): SExpr = xs match {
  case Nil => ()
  case x :: Nil => eval(env, x)
  case x :: xs => {
    eval(env, x)
    evalBegin(env, xs)
  }
}