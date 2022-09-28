package littlejian.ext

import littlejian._

def run(goal: Goal)(implicit searcher: Searcher): Stream[State] = searcher.run(State.empty, goal)

implicit class StateOps(self: State)(implicit searcher: Searcher) {
  def execute(goal: Goal): Stream[State] = searcher.run(self, goal)
}

def printSpace(root: Var[_], space: Seq[State]): String = ???

def run[T](block: VarOr[T] => Goal)(implicit unifier: Unifier[T], searcher: Searcher): String = {
  val root = hole
  printSpace(root, run(block(root)))
}

def run[T](n: Int, block: VarOr[T] => Goal)(implicit unifier: Unifier[T], searcher: Searcher): String = {
  val root = hole
  printSpace(root, run(block(root)).take(n))
}