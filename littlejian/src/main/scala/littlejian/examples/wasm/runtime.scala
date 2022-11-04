package littlejian.examples.wasm

import littlejian._
import littlejian.ext._
import littlejian.data._

enum RuntimeValue derives Unify :
  case I32(x: VarOr[Int])
  case I64(x: VarOr[Long])
  case F32(x: VarOr[Float])
  case F64(x: VarOr[Double])
// TODO: 128

implicit class RuntimeValueOps(self: RuntimeValue) {
  def getType: ValueType = self match {
    case RuntimeValue.I32(_) => ValueType.I32
    case RuntimeValue.I64(_) => ValueType.I64
    case RuntimeValue.F32(_) => ValueType.F32
    case RuntimeValue.F64(_) => ValueType.F64
  }
}

implicit object ForceRuntimeValue extends Force[RuntimeValue] {
  override def force(self: VarOr[RuntimeValue]): GoalWith[RuntimeValue] = GoalWith(k => conde(
    for {
      x <- self.is[Int](RuntimeValue.I32(_))
      _ <- k(RuntimeValue.I32(x))
    } yield (),
    for {
      x <- self.is[Long](RuntimeValue.I64(_))
      _ <- k(RuntimeValue.I64(x))
    } yield (),
    for {
      x <- self.is[Float](RuntimeValue.F32(_))
      _ <- k(RuntimeValue.F32(x))
    } yield (),
    for {
      x <- self.is[Double](RuntimeValue.F64(_))
      _ <- k(RuntimeValue.F64(x))
    } yield (),
  ))
}

implicit class VarOrRuntimeValueOps(self: VarOr[RuntimeValue]) {
  def getType: Rel[ValueType] = self.force.map(_.getType)
}

type UInt = Int

type Locals = Mapping[UInt, RuntimeValue]

final case class Activation(pc: VarOr[UInt], functionIndex: VarOr[UInt], locals: VarOr[Locals]) derives Unify

type ActivationStack = LList[Activation]

enum RuntimeError derives Unify:
  case NotFound(str: VarOr[Str])
  case ExpectCodeSection
  case ExpectValueStack
  case ExpectLabelStack
  case ExpectActivationStack
  case Unimplemented
  case InvalidArgs(xs: VarOr[LList[ValueType]], ys: VarOr[LList[ValueType]])

final case class Function(params: VarOr[LList[ValueType]], returns: VarOr[LList[ValueType]], code: VarOr[LList[Inst]]) derives Unify {
  def getType: FuncType = FuncType(params, returns)
}

type FunctionTable = LList[Function]