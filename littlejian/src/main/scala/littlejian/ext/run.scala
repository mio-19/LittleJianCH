package littlejian.ext

import littlejian._

def run(goal: Goal)(implicit searcher: Searcher): Seq[State] = searcher.run(State.empty, goal)

implicit class StateOps(self: State)(implicit searcher: Searcher) {
  def execute(goal: Goal): Seq[State] = searcher.run(self, goal)
}

def printRoot(root: Var[_], state: State): String = prettyPrintContext.callWith(new PrettyPrintContext(state.eq.subst)) {
  root.toString
}

def printConstraints(state: State): String = "" // TODO

def printState(root: Var[_], state: State): String = printRoot(root, state) + printConstraints(state)

// useful for maybe infinite spaces
def displaySpace(root: Var[_], space: Seq[State]): Unit = space.foreach(s => println(printState(root, s)))

def run[T](block: VarOr[T] => Goal)(implicit unifier: Unifier[T], searcher: Searcher): Seq[String] = {
  val root = new Var[T]()
  run(block(root)).map(printState(root, _))
}

def runDisplay[T](block: VarOr[T] => Goal)(implicit unifier: Unifier[T], searcher: Searcher): Unit = {
  val root = new Var[T]()
  displaySpace(root, run(block(root)))
}
