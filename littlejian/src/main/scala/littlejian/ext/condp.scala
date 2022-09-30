package littlejian.ext

import littlejian._

def condp(keys: Seq[String]*)(clauses: => (String, Goal)*): Goal = GoalDelay(???)

def condp[T](keys: Seq[String]*)(clauses: => (String, Rel[T])*)(implicit unifier: Unifier[T]): Rel[T] = {
  val v = new Var[T]
  GoalWith(GoalDelay {
    condp(keys *)(clauses.map { case (id, rel) => (id, GoalConj(rel.goal, rel.x === v)) } *)
  }, v)
}