package littlejian.examples.miniMal

import littlejian.data._
import littlejian._
import littlejian.ext._

def evalo(ast: VarOr[Data], envId: VarOr[EnvId], envIn: VarOr[WholeEnv], counterIn: VarOr[EnvVar], counterOut: VarOr[EnvVar], envOut: VarOr[WholeEnv]): Rel[Data] = conde(
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
  for {
    v <- ast.is[Data](LList("`", _))
    _ <- counterIn === counterOut && envIn === envOut
  } yield v,
  for {
    (params, body) <- ast.is[Data, Data](LList("fn", _, _))
  } yield ???,
  for {
    (clauses, body) <- ast.is[Data, Data](LList("let", _, _))
  } yield ???,
  for {
    xs <- ast.is[LList[Data]]("do" :: _)
  } yield ???,
)