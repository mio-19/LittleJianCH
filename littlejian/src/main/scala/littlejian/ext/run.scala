package littlejian.ext

import littlejian._

inline def run(goal: Goal)(implicit searcher: Searcher): Seq[State] = searcher.run(State.empty, goal)

implicit class StateOps(self: State)(implicit searcher: Searcher) {
  inline def execute(goal: Goal): Seq[State] = searcher.run(self, goal)
}

def printState(root: Var[_], state: State): String = prettyPrintContext.callWith(new PrettyPrintContext(state.eq.subst)) {
  val r = root.toString
  val c = state.printConstraints
  if (c.isEmpty) r else r + "\n" + c
}

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
