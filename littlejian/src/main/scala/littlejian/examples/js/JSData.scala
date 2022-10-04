package littlejian.examples.js

import littlejian.data._

type JSData0[JSData] = Unit | Nat | String | Boolean | LList[JSData]
type JSData = Fix0[JSData0]