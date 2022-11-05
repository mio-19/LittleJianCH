package littlejian.examples.wasm
import littlejian._
import littlejian.ext._
import littlejian.data._
sealed trait Inst derives Unify
object Inst {
  case object Reserved extends Inst derives Unify
  final case class Prefix(x: VarOr[UIntN]) extends Inst derives Unify
  case object Unreachable extends Inst derives Unify
  case object Nop extends Inst derives Unify
  final case class Block(x: VarOr[BlockType]) extends Inst derives Unify
  final case class Loop(x: VarOr[BlockType]) extends Inst derives Unify
  final case class If(x: VarOr[BlockType]) extends Inst derives Unify
  case object Else extends Inst derives Unify
  case object End extends Inst derives Unify
  final case class Br(x: VarOr[UIntN]) extends Inst derives Unify
  final case class BrIf(x: VarOr[UIntN]) extends Inst derives Unify
  final case class BrTable(xs: VarOr[LList[UIntN]], x: VarOr[UIntN]) extends Inst derives Unify
  case object Return extends Inst derives Unify
  final case class Call(x: VarOr[UIntN]) extends Inst derives Unify
  final case class CallIndirect(x: VarOr[UIntN], y: VarOr[UIntN]) extends Inst derives Unify
  case object Drop extends Inst derives Unify
  case object Select extends Inst derives Unify
  final case class GetLocal(x: VarOr[UIntN]) extends Inst derives Unify
  final case class SetLocal(x: VarOr[UIntN]) extends Inst derives Unify
  final case class TeeLocal(x: VarOr[UIntN]) extends Inst derives Unify
  final case class GetGlobal(x: VarOr[UIntN]) extends Inst derives Unify
  final case class SetGlobal(x: VarOr[UIntN]) extends Inst derives Unify
  final case class I32Load(x: VarOr[IntSigned], y: VarOr[IntSigned]) extends Inst derives Unify
  final case class I64Load(x: VarOr[IntSigned], y: VarOr[IntSigned]) extends Inst derives Unify
  final case class F32Load(x: VarOr[IntSigned], y: VarOr[IntSigned]) extends Inst derives Unify
  final case class F64Load(x: VarOr[IntSigned], y: VarOr[IntSigned]) extends Inst derives Unify
  final case class I32Load8S(x: VarOr[IntSigned], y: VarOr[IntSigned]) extends Inst derives Unify
  final case class I32Load8U(x: VarOr[IntSigned], y: VarOr[IntSigned]) extends Inst derives Unify
  final case class I32Load16S(x: VarOr[IntSigned], y: VarOr[IntSigned]) extends Inst derives Unify
  final case class I32Load16U(x: VarOr[IntSigned], y: VarOr[IntSigned]) extends Inst derives Unify
  final case class I64Load8S(x: VarOr[IntSigned], y: VarOr[IntSigned]) extends Inst derives Unify
  final case class I64Load8U(x: VarOr[IntSigned], y: VarOr[IntSigned]) extends Inst derives Unify
  final case class I64Load16S(x: VarOr[IntSigned], y: VarOr[IntSigned]) extends Inst derives Unify
  final case class I64Load16U(x: VarOr[IntSigned], y: VarOr[IntSigned]) extends Inst derives Unify
  final case class I64Load32S(x: VarOr[IntSigned], y: VarOr[IntSigned]) extends Inst derives Unify
  final case class I64Load32U(x: VarOr[IntSigned], y: VarOr[IntSigned]) extends Inst derives Unify
  final case class I32Store(x: VarOr[IntSigned], y: VarOr[IntSigned]) extends Inst derives Unify
  final case class I64Store(x: VarOr[IntSigned], y: VarOr[IntSigned]) extends Inst derives Unify
  final case class F32Store(x: VarOr[IntSigned], y: VarOr[IntSigned]) extends Inst derives Unify
  final case class F64Store(x: VarOr[IntSigned], y: VarOr[IntSigned]) extends Inst derives Unify
  final case class I32Store8(x: VarOr[IntSigned], y: VarOr[IntSigned]) extends Inst derives Unify
  final case class I32Store16(x: VarOr[IntSigned], y: VarOr[IntSigned]) extends Inst derives Unify
  final case class I64Store8(x: VarOr[IntSigned], y: VarOr[IntSigned]) extends Inst derives Unify
  final case class I64Store16(x: VarOr[IntSigned], y: VarOr[IntSigned]) extends Inst derives Unify
  final case class I64Store32(x: VarOr[IntSigned], y: VarOr[IntSigned]) extends Inst derives Unify
  final case class CurrentMemory(x: VarOr[UIntN]) extends Inst derives Unify
  final case class GrowMemory(x: VarOr[UIntN]) extends Inst derives Unify
  final case class I32Const(x: VarOr[IntSigned]) extends Inst derives Unify
  final case class I64Const(x: VarOr[Int64Signed]) extends Inst derives Unify
}
