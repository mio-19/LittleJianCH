package littlejian.examples.wasm

import littlejian._
import littlejian.data._
import littlejian.ext.*
import scala.language.implicitConversions


enum Type derives Unify :
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

object Type {
  def from(x: Byte): Type = x match {
    case 0x7F => Type.I32
    case 0x7E => Type.I64
    case 0x7D => Type.F32
    case 0x7C => Type.F64
    case _ => throw new IllegalArgumentException(s"Unknown type: $x")
  }

  def from(x: VarOr[Byte]): Rel[Type] = x.switch(
    0x7F.toByte -> Type.I32,
    0x7E.toByte -> Type.I64,
    0x7D.toByte -> Type.F32,
    0x7C.toByte -> Type.F64
  )
}

type UIntN = Int