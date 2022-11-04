package littlejian.examples.wasm

import littlejian._
import littlejian.data._
import littlejian.ext.*
import scala.language.implicitConversions


enum ValueType derives Unify :
  case I32
  case I64
  case F32
  case F64
  case Unknown

enum BlockType derives Unify :
  case I32
  case I64
  case F32
  case F64
  case Empty

object ValueType {
  def from(x: Byte): ValueType = x match {
    case 0x7F => ValueType.I32
    case 0x7E => ValueType.I64
    case 0x7D => ValueType.F32
    case 0x7C => ValueType.F64
    case _ => throw new IllegalArgumentException(s"Unknown type: $x")
  }

  def from(x: VarOr[Byte]): Rel[ValueType] = x.switch(
    0x7F.toByte -> ValueType.I32,
    0x7E.toByte -> ValueType.I64,
    0x7D.toByte -> ValueType.F32,
    0x7C.toByte -> ValueType.F64
  )
}

type UIntN = Int