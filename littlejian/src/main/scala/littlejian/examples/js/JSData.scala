package littlejian.examples.js

import littlejian.data._
import littlejian._

type JSData0[JSData] = Unit | Nat | String | Boolean | LList[JSData]
type JSData = Fix[JSData0]
object Helper$SExp extends FixHelper[JSData0]
import Helper$SExp._

def list(xs: VarOr[JSData]*): JSData = LList(xs*)