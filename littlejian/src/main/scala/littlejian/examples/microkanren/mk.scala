package littlejian.examples.microkanren

import littlejian._
import littlejian.data._

final case class MKVar(id: VarOr[Nat]) extends Product1[VarOr[Nat]]

implicit val U$MKVar: Unifier[MKVar] = U$Product

final case class MKPair(a: VarOr[MKData], b: VarOr[MKData]) extends Product2[VarOr[MKData], VarOr[MKData]]

implicit val U$MKPair: Unifier[MKPair] = U$Product

type MKData = MKVar | Unit | String | MKPair

implicit val U$MKData: Unifier[MKData] = U$Union[MKVar, Unit, String, MKPair]