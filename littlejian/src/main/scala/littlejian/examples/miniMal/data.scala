package littlejian.examples.miniMal

import littlejian.*
import littlejian.data.*

import scala.language.implicitConversions

sealed trait LListData extends LList[Data]

final class EmptyList extends LEmpty[Data] with LListData

final class NonEmptyList(head: VarOr[Data], tail: VarOr[LListData]) extends LCons[Data](head, tail) with LListData

implicit def varConv1(x: VarOr[LListData]): VarOr[LList[Data]] = x.asInstanceOf[VarOr[LList[Data]]]
implicit def varConv2(x: VarOr[LList[Data]]): VarOr[LListData] = x.asInstanceOf[VarOr[LListData]]
implicit def toLListData(x: LList[Data]): LListData = x match {
  case LEmpty() => new EmptyList
  case LCons(head, tail) => new NonEmptyList(head, tail)
}
given U$LListData: Unifier[LListData] = U$LList(U$Data).asInstanceOf[Unifier[LListData]]

type Data = (String | Int32 | Boolean | LListData) | Closure

final case class Closure(envId: VarOr[EnvVar], params: VarOr[LList[String]], vararg: VarOr[Option[String]], ast: VarOr[Data]) derives Unifier

given U$Data: Unifier[Data] = U$Union(U$Union[String, Int32, Boolean, LListData], implicitly[Unifier[Closure]])


type EnvVar = BinaryNat

final case class EnvEntry(env: VarOr[EnvVar], id: VarOr[String], value: VarOr[Data]) derives Unifier

final case class WholeEnv(env: VarOr[LList[EnvEntry]]) derives Unifier

def evalo(ast: VarOr[Data], envIn: VarOr[WholeEnv], counterIn: VarOr[EnvVar], counterOut: VarOr[EnvVar], envOut: VarOr[WholeEnv]): Goal = ???