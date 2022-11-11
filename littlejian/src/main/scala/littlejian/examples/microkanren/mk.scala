package littlejian.examples.microkanren

import littlejian._
import littlejian.data._
import littlejian.ext._

// https://github.com/jasonhemann/micro-in-mini/blob/master/micro-in-mini.rkt

def applyEnvo(env: VarOr[mkMap], y: VarOr[MKData]): Rel[MKData] = for {
  (key, value, tail) <- env.is(MKMapCons(_, _, _))
  result <- compare(key, y) {
    conde(
      for {
        (x, exp2) <- value.is(MKRec(_, _))
        result <- microo(list("lambda", list(x), exp2), env)
      } yield result,
      for {
        b <- value.is(MKReg(_))
      } yield b
    )
  } {
    applyEnvo(tail, y)
  }
} yield result

def bindo(xs: VarOr[MKData], g: VarOr[MKData]): Rel[MKData] = conde(
  begin(xs === (), ()),
  for {
    _ <- xs.is(MKThunk(_, _))
  } yield MKThunk(MKThunkKind.Bind, Vector(xs, g)),
  for {
    (a, d) <- xs.is(MKPair(_, _))
    (aa, da) <- a.is(MKPair(_, _))
    xs1 <- runGoalo(g, aa, da)
    xs2 <- bindo(d, g)
    result <- mpluso(xs1, xs2)
  } yield result
)

def mpluso(xs: VarOr[MKData], ys: VarOr[MKData]): Rel[MKData] = ???

def runGoalo(g: VarOr[MKData], s: VarOr[MKData], c: VarOr[MKData]): Rel[MKData] = ???


def microo(x: VarOr[MKData], env: VarOr[mkMap]): Rel[MKData] = ???

def walko(x: VarOr[MKData], subst: VarOr[mkMap]): Rel[MKData] = x.caseOnType(MKVar(_)) { v =>
  ???
} {
  ???
}
