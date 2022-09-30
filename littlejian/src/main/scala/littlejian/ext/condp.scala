package littlejian.ext

import littlejian._

final case class Walker(f: Any => Any) {
  def apply[T](x: VarOr[T]): VarOr[T] = f(x).asInstanceOf[VarOr[T]]
}

val UseMaybe = "use-maybe"

private def collect(subst: Subst, keys: Seq[Walker => Seq[String]]*): Seq[String] =
  if (keys.isEmpty) Seq.empty
  else {
    val walker = Walker(subst.walk)
    val ulos = keys.head.flatMap(_ (walker))
    if (ulos.contains(UseMaybe)) ulos ++ collect(subst, keys.tail *) else ulos
  }

def condp(keys: Seq[Walker => Seq[String]]*)(clauses: => (String, Goal)*): Goal = GoalDelay(???)

def condp[T](keys: Seq[Walker => Seq[String]]*)(clauses: => (String, Rel[T])*)(implicit unifier: Unifier[T]): Rel[T] = {
  val v = new Var[T]
  GoalWith(GoalDelay {
    condp(keys *)(clauses.map { case (id, rel) => (id, GoalConj(rel.goal, rel.x === v)) } *)
  }, v)
}