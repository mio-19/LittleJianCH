package littlejian.search

import scala.collection.parallel.immutable.ParVector
import littlejian._

implicit object GradualSearcher extends Searcher {
  final case class World(state: State, goals: ParVector[Goal]) {
    def run: (Seq[State], SStream[World]) = ???
    def exec: SStream[State] = {
      val (ok, rest) = this.run
      mplus(SStream.from(ok), flatten(rest.map(_.exec)))
    }
  }

  override def run(state: State, goal: Goal): Stream[State] = World(state, ParVector(goal)).exec.toStream
}
