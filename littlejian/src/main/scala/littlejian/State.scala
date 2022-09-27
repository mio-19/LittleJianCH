package littlejian

import scala.collection.parallel.immutable.{ParSeq, ParHashMap}

type EqState = Subst

type NotEqState = ParSeq[Subst]

type PredTypeState = ParHashMap[_, PredTypeTag]

type PredNotTypeState = ParHashMap[_, PredTypeTag]

final case class State(eq: EqState, notEq: NotEqState, predType: PredTypeState, predNotType: PredNotTypeState)
