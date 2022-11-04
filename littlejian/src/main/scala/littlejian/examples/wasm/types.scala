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

final case class FuncType(params: VarOr[LList[ValueType]], results: VarOr[LList[ValueType]]) derives Unify

final case class FunctionSection(types: VarOr[LList[UInt]]) derives Unify

enum ExternalKind derives Unify :
  case Function
  case Table
  case Memory
  case Global

object ExternalKind {
  def from(x: UIntN): ExternalKind = x match {
    case 0x00 => ExternalKind.Function
    case 0x01 => ExternalKind.Table
    case 0x02 => ExternalKind.Memory
    case 0x03 => ExternalKind.Global
    case _ => throw new IllegalArgumentException(s"Unknown ExternalKind: $x")
  }

  def from(x: VarOr[UIntN]): Rel[ExternalKind] = x.switch(
    0x00 -> ExternalKind.Function,
    0x01 -> ExternalKind.Table,
    0x02 -> ExternalKind.Memory,
    0x03 -> ExternalKind.Global
  )
}

final case class ExportEntry(fieldStr: VarOr[Str], kind: VarOr[ExternalKind], index: VarOr[UInt]) derives Unify

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


final case class DataSegment(index: VarOr[UInt], offset: VarOr[UInt], data: VarOr[LList[U8]]) derives Unify

type DataSection = LList[DataSegment]

final case class LocalEntry(count: VarOr[UInt], valueType: VarOr[ValueType]) derives Unify

final case class FunctionBody(locales: VarOr[LList[LocalEntry]], code: VarOr[LList[Inst]]) derives Unify

type CodeSection = LList[FunctionBody]