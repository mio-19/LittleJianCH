package littlejian

trait Searcher {
  def run(state: State, goal: Goal): Stream[State]
}

implicit class StateOps(self: State)(implicit searcher: Searcher) {
  def execute(goal: Goal): Stream[State] = searcher.run(self, goal)
}