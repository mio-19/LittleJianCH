package littlejian.ext

import littlejian._

def run(goal: Goal)(implicit searcher: Searcher): Stream[State] = searcher.run(State.empty, goal)

implicit class StateOps(self: State)(implicit searcher: Searcher) {
  def execute(goal: Goal): Stream[State] = searcher.run(self, goal)
}

def printRoot(root: Var[_], state: State): String = PrettyPrintContext.callWith(new PrettyPrintContext(state.eq.subst)) {
  root.toString
}

def printConstraints(state: State): String = "" // TODO

def printState(root: Var[_], state: State): String = printRoot(root, state) + printConstraints(state)

def printSpace(root: Var[_], space: Seq[State]): String = space.map(s => printState(root, s) + "\n").fold("")(_ + _)

// useful for maybe infinite spaces
def displaySpace(root: Var[_], space: Stream[State]): Unit = space.foreach(s => println(printState(root, s)))

def run[T](block: VarOr[T] => Goal)(implicit unifier: Unifier[T], searcher: Searcher): String = {
  val root = hole
  printSpace(root, run(block(root)))
}

def runDisplay[T](block: VarOr[T] => Goal)(implicit unifier: Unifier[T], searcher: Searcher): Unit = {
  val root = hole
  displaySpace(root, run(block(root)))
}

def run[T](n: Int, block: VarOr[T] => Goal)(implicit unifier: Unifier[T], searcher: Searcher): String = {
  val root = hole
  printSpace(root, run(block(root)).take(n))
}