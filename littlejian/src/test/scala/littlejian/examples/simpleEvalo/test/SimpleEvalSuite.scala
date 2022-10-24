package littlejian.examples.simpleEvalo.test

import littlejian.*
import littlejian.data.*
import littlejian.data.sexp.*
import littlejian.examples.simpleEvalo.evalExpo
import littlejian.ext.*
import littlejian.search.BFSimp

class SimpleEvalSuite extends munit.FunSuite {
  val quineC = SExp.parse("((lambda (x)\n(list x (list (quote quote) x)))\n(quote\n(lambda (x)\n(list x (list (quote quote) x)))))")
  test("quineC") {
    assertEquals((run[SExp] { x => x === "Success" && evalExpo(quineC, (), quineC) }).head, "Success")
  }
  test("basics") {
    if(false) {
      assertEquals(Set.from(run {
        for {
          x <- fresh[SExp]
          _ <- evalExpo(x, (), x)
        } yield x
      }), Set())
      assertEquals(Set.from(run {
        for {
          (p, q, r) <- fresh[SExp, SExp, SExp]
          _ <- p =/= q && p =/= r && q =/= r
          _ <- evalExpo(p, (), q) && evalExpo(q, (), r) && evalExpo(r, (), p)
        } yield list(p, q, r): VarOr[SExp]
      }), Set())
    }
  }
}
