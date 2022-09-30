package littlejian.examples.microkanren

import littlejian._
import littlejian.data.Nat

final case class MKVar(id: VarOr[Nat])

final case class MKPair(a: VarOr[MKData], b: VarOr[MKData])

type MKData = MKVar | Unit | String | MKPair