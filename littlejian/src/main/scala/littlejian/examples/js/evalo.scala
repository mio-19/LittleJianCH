package littlejian.examples.js
import littlejian._
import littlejian.ext._


// https://github.com/Artish357/RelateJS/blob/master/evalo.rkt

def evalEnvo(expr: VarOr[JSData], env: VarOr[JSData], value: VarOr[JSData], store: VarOr[JSData], store1: VarOr[JSData], nextAddr: VarOr[JSData], nextAddr1: VarOr[JSData]): Goal = conde(
  // Atomic values (Section 2.2.1)
  (expr === value && store === store1 && nextAddr === nextAddr1) && conde(
    for {
      _ <- expr.is[JSData](jrawnum)
    } yield (),
    for {
      _ <- expr.is[JSData](jobj)
    } yield (),
    for {
      _ <- expr.is[JSData](jbool)
    } yield (),
    for {
      _ <- expr.is[JSData](jrawstr)
    } yield (),
    for {
      _ <- expr === jundef || expr === jnul
    } yield ()
  )
)
