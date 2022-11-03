package littlejian.examples.wasm
import littlejian._
import littlejian.ext._
import littlejian.data._

enum RuntimeValue derives Unify:
  case I32(x: VarOr[Int])
  case I64(x: VarOr[Int])
  case F32(x: VarOr[Float])
  case F64(x: VarOr[Double])
  // TODO: 128


