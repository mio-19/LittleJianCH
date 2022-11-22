package littlejian.racket

import littlejian.data.sexp._
import scala.collection.mutable.HashMap

final case class Env(stack: List[HashMap[String, SExp]]) {
  def lookup(x: String): Option[SExp] = {
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

  def update(key: String, value: SExp): Unit = stack.head.update(key, value)
}

object LIST {
  def apply(xs: SExp*) = list(xs*)
  def unapplySeq(x: SExp): Option[Seq[SExp]] = x match {
    case () => Some(Seq())
    case Cons(x:SExp, xs:SExp) => unapplySeq(xs).map(x+:_)
    case _ => None
  }
}

def eval(env: Env, exp: SExp): SExp = exp match {
  case list("define", name: String, body: SExp) => {
    env.update(name, eval(env, body))
    ()
  }
  case LIST("begin", xs@ _* ) => evalBegin(env, xs.toList)
}

def evalBegin(env: Env, xs: List[SExp]): SExp = xs match {
  case Nil => ()
  case x :: Nil => eval(env, x)
  case x :: xs => {
    eval(env, x)
    evalBegin(env, xs)
  }
}