package littlejian.examples.miniMal

import littlejian.*
import littlejian.ext.*
import littlejian.data.*

import scala.annotation.targetName
import scala.language.implicitConversions

sealed trait LListData

implicit def LListData2LList(x: LListData): LList[Data] = x.asInstanceOf

final class EmptyList extends LEmpty[Data] with LListData

final class NonEmptyList(head: VarOr[Data], tail: VarOr[LListData]) extends LCons[Data](head, tail) with LListData

implicit def varConv1(x: VarOr[LListData]): VarOr[LList[Data]] = x.asInstanceOf[VarOr[LList[Data]]]
implicit def varConv2(x: VarOr[LList[Data]]): VarOr[LListData] = x.asInstanceOf[VarOr[LListData]]
implicit def toLListData(x: LList[Data]): LListData = x match {
  case LEmpty() => new EmptyList
  case LCons(head, tail) => new NonEmptyList(head, tail)
}
given U$LListData: Unify[LListData] = U$LList(U$Data).asInstanceOf[Unify[LListData]]

type Data = (String | Int32 | Boolean) | LListData | (Closure | Macro) | Unit

final case class Params(params: VarOr[LList[String]], vararg: VarOr[Option[VarOr[String]]]) derives Unify

final case class Closure(env: VarOr[WholeEnv], envId: VarOr[EnvId], params: VarOr[Params], ast: VarOr[Data]) derives Unify

final case class Macro(f: VarOr[Data]) derives Unify

given U$Data: Unify[Data] = U$Union[String, Int32, Boolean, LEmpty[Data], LCons[Data], Closure, Macro, Unit].asInstanceOf[Unify[Data]]


type EnvVar = BinaryNat
type EnvId = LList[EnvVar]

final case class EnvEntry(envId: VarOr[EnvVar], id: VarOr[String], value: VarOr[Data]) derives Unify

type WholeEnv = LList[EnvEntry]

def setEnv(envId: VarOr[EnvVar], id: VarOr[String], value: VarOr[Data], envIn: VarOr[WholeEnv]): WholeEnv = EnvEntry(envId, id, value) +: envIn
def setEnv(envId: VarOr[EnvVar], id: VarOr[String], value: VarOr[Data], envIn: VarOr[WholeEnv], envOut: VarOr[WholeEnv]): Goal = envOut === setEnv(envId, id, value, envIn)
@targetName("setEnv2") def setEnv(envId: VarOr[EnvId], id: VarOr[String], value: VarOr[Data], envIn: VarOr[WholeEnv], envOut: VarOr[WholeEnv]): Goal = for {
  (eid, _) <- envId.is[EnvVar, EnvId](_ :: _)
  _ <- setEnv(eid, id, value, envIn, envOut)
} yield ()
@targetName("setEnv2") def setEnv(envId: VarOr[EnvId], id: VarOr[String], value: VarOr[Data], envIn: VarOr[WholeEnv]): Rel[WholeEnv] = for {
  (eid, _) <- envId.is[EnvVar, EnvId](_ :: _)
  envOut <- setEnv(eid, id, value, envIn)
} yield envOut

def envGet(envId: VarOr[EnvVar], id: VarOr[String], env: VarOr[WholeEnv]): Rel[Option[VarOr[Data]]] = conde(
  env.isEmpty >> None,
  for {
    (head, tail) <- env.is[EnvEntry, WholeEnv](_ :: _)
    (eid0, id0, val0) <- head.is[EnvVar, String, Data](EnvEntry(_, _, _))
    result <- compare(eid0, envId) {
      compare(id0, id) {
        Some(val0)
      } {
        envGet(envId, id, tail)
      }
    } {
      envGet(envId, id, tail)
    }
  } yield result
)
@targetName("envGet2") def envGet(envId: VarOr[EnvId], id: VarOr[String], env: VarOr[WholeEnv]): Rel[Data] = for {
  (eid, tail) <- envId.is[EnvVar, EnvId](_ :: _)
  r <- envGet(eid, id, env)
  ret <- r.elim {
    envGet(tail, id, env)
  }{ v => Rel(v) }
} yield ret
