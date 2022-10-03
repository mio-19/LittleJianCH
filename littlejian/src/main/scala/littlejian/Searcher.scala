package littlejian

trait Searcher {
  def run(state: State, goal: Goal): Seq[State]
}
