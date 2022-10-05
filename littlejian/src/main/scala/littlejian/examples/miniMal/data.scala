package littlejian.examples.miniMal

import littlejian._
import littlejian.data._

sealed trait LListData extends LList[Data]

final class EmptyList extends LEmpty[Data] with LListData

final class NonEmptyList(head: VarOr[Data], tail: VarOr[LListData]) extends LCons[Data](head, tail) with LListData

type Data = String | Int32 | Boolean | LListData



final case class EnvVar(id: VarOr[BinaryNat]) derives Unifier

final case class EnvSet(env: VarOr[EnvVar], id: VarOr[String], value: VarOr[Data])