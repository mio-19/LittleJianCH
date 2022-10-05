package littlejian.examples.miniMal

import littlejian.*
import littlejian.ext._
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

type Data = (String | Int32 | Boolean | LListData) | (Closure | Macro) | Unit

final case class Closure(envId: VarOr[EnvVar], params: VarOr[LList[String]], vararg: VarOr[Option[String]], ast: VarOr[Data]) derives Unifier

final case class Macro(f: VarOr[Data]) derives Unifier

given U$Data: Unifier[Data] = U$Union(U$Union[String, Int32, Boolean, LListData], U$Union[Closure, Macro], U$Unit)


type EnvVar = BinaryNat

final case class EnvEntry(envId: VarOr[EnvVar], id: VarOr[String], value: VarOr[Data]) derives Unifier

type WholeEnv = LList[EnvEntry]

def setEnv(envId: VarOr[EnvVar], id: VarOr[String], value: VarOr[Data], envIn: VarOr[WholeEnv]): WholeEnv = EnvEntry(envId, id, value) +: envIn
def setEnv(envId: VarOr[EnvVar], id: VarOr[String], value: VarOr[Data], envIn: VarOr[WholeEnv], envOut: VarOr[WholeEnv]): Goal = envOut === setEnv(envId, id, value, envIn)

def evalo(ast: VarOr[Data], envId: VarOr[EnvVar], envIn: VarOr[WholeEnv], counterIn: VarOr[EnvVar], counterOut: VarOr[EnvVar], envOut: VarOr[WholeEnv]): Rel[Data] = conde(
  for {
    (id, a) <- ast.is[Data, Data](LList("def", _, _))
    ids <- id.cast[String]
    envOut0 <- fresh[WholeEnv]
    v <- evalo(a, envId, envIn, counterIn, counterOut, envOut0)
    _ <- setEnv(envId, ids, v, envOut0, envOut)
  } yield (),
  for {
    f <- ast.is[Data](LList("~", _))
    v <- evalo(f, envId, envIn, counterIn, counterOut, envOut)
  } yield Macro(v),
)