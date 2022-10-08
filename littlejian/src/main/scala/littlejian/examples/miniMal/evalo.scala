package littlejian.examples.miniMal

import littlejian.data._
import littlejian._
import littlejian.ext._

def resolveLet(clauses: VarOr[Data]/* | VarOr[LList[Data]]*/, blockEnvId: VarOr[EnvId], envIn: VarOr[WholeEnv], counterIn: VarOr[EnvVar], counterOut: VarOr[EnvVar], envOut: VarOr[WholeEnv]): Goal = conde(
  (clauses === LList.empty) && (envIn === envOut),
  for {
    (id, v, tail) <- clauses.is[Data, Data, LList[Data]]((id: VarOr[Data], v: VarOr[Data], tail: VarOr[LList[Data]]) => id :: (v :: tail))
    id <- id.cast[String]
    counterOut0 <- fresh[EnvVar]
    envOut0 <- fresh[WholeEnv]
    evaled <- evalo(v, blockEnvId, envIn, counterIn, counterOut0, envOut0)
    envOut1 <- setEnv(blockEnvId, id, evaled, envOut0)
    _ <- resolveLet(tail.asInstanceOf[VarOr[Data]], blockEnvId, envOut1, counterOut0, counterOut, envOut)
  } yield ()
)

def freshEnv(envId: VarOr[EnvId], counterIn: VarOr[EnvVar], counterOut: VarOr[EnvVar]): Rel[EnvId] = for {
  _ <- counterIn.succ === counterOut
} yield (counterIn :: envId)

def parseParams(xs: VarOr[Data]/* | VarOr[LList[Data]]*/): Rel[Params] = conde(
  (xs === LList.empty) >> Rel(Params(LList.empty, None)),
  for {
    x <- xs.cast[String]
  } yield Params(LList.empty, Some(x)),
  for {
    (x, xs) <- xs.is[Data, LList[Data]](_ :: _)
    x <- x.cast[String]
    params <- parseParams(xs.asInstanceOf[VarOr[Data]])
    (xs0, vararg) <- params.is(Params(_, _))
  } yield Params(x :: xs0, vararg)
)

def runDo(ast: VarOr[LList[Data]], envId: VarOr[EnvId], envIn: VarOr[WholeEnv], counterIn: VarOr[EnvVar], counterOut: VarOr[EnvVar], envOut: VarOr[WholeEnv]): Rel[Data] = for {
  (head, tail) <- ast.is[Data, LList[Data]](_ :: _)
  result <- tail.elim {
    evalo(head, envId, envIn, counterIn, counterOut, envOut)
  } {
    (_, _) => for {
      counter0 <- fresh[EnvVar]
      env0 <- fresh[WholeEnv]
      _ <- runDo(tail, envId, envIn, counterIn, counter0, env0)
      result <- evalo(head, envId, env0, counter0, counterOut, envOut)
    } yield result
  }
} yield result

def notKeywordo(x: VarOr[Data]): Goal = x =/= "def" && x =/= "~" && x =/= "`" && x =/= "fn" && x =/= "let" && x =/= "do" && x =/= "if"

def evalo(ast: VarOr[Data], envId: VarOr[EnvId], envIn: VarOr[WholeEnv], counterIn: VarOr[EnvVar], counterOut: VarOr[EnvVar], envOut: VarOr[WholeEnv]): Rel[Data] = conde(
  for {
    id <- ast.cast[String]
    value <- envGet(envId, id, envIn)
    _ <- counterOut === counterIn && envOut === envIn
  } yield value,
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
    params <- parseParams(params)
    closureEnvId <- freshEnv(envId, counterIn, counterOut)
    _ <- envIn == envOut
  } yield Closure(envIn, closureEnvId, params, body),
  for {
    (clauses, body) <- ast.is[Data, Data](LList("let", _, _))
    counter0 <- fresh[EnvVar]
    blockEnvId <- freshEnv(envId, counterIn, counter0)
    counter1 <- fresh[EnvVar]
    env0 <- fresh[WholeEnv]
    _ <- resolveLet(clauses, blockEnvId, envIn, counter0, counter1, env0)
    v <- evalo(body, blockEnvId, env0, counter1, counterOut, envOut)
  } yield v,
  for {
    xs <- ast.is[LList[Data]]("do" :: _)
    result <- runDo(xs, envId, envIn, counterIn, counterOut, envOut)
  } yield result,
  for {
    (cond, ifTrue, ifFalse) <- ast.is[Data, Data, Data](LList("if", _, _, _))
    counter0 <- fresh[EnvVar]
    env0 <- fresh[WholeEnv]
    cond <- evalo(cond, envId, envIn, counterIn, counter0, env0)
    cond <- cond.cast[Boolean]
    v <- cond.elim {
      evalo(ifTrue, envId, env0, counter0, counterOut, envOut)
    } {
      evalo(ifFalse, envId, env0, counter0, counterOut, envOut)
    }
  } yield v,
  for {
    (x, args) <- ast.is[Data, LList[Data]](_ :: _)
    _ <- notKeywordo(x)
  } yield ???
)