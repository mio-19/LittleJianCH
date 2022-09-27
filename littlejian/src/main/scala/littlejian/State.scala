package littlejian

import scala.collection.parallel.immutable.{ParSeq, ParHashMap}

final case class EqState(subst: Subst)

object EqState {
  val empty: EqState = EqState(Subst.empty)
}

final case class NotEqState(clauses: ParSeq /*conj*/ [Subst /*disj not eq*/ ])

object NotEqState {
  val empty: NotEqState = NotEqState(ParSeq.empty)
}

final case class PredTypeState(xs: ParHashMap[_, PredTypeTag])

object PredTypeState {
  val empty: PredTypeState = PredTypeState(ParHashMap.empty)
}

final case class PredNotTypeState(xs: ParHashMap[_, PredTypeTag])

object PredNotTypeState {
  val empty: PredNotTypeState = PredNotTypeState(ParHashMap.empty)
}

final case class State(eq: EqState, notEq: NotEqState, predType: PredTypeState, predNotType: PredNotTypeState)

object State {
  val empty: State = State(eq = EqState.empty, notEq = NotEqState.empty, predType = PredTypeState.empty, predNotType = PredNotTypeState.empty)
}