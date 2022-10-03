package littlejian.examples.simpleEvalo.test

import littlejian.*
import littlejian.data.*
import littlejian.data.sexp.*
import littlejian.examples.simpleEvalo.evalExpo
import littlejian.ext.*
import littlejian.unifier.*
import littlejian.search.NaiveSearcher

class SimpleEvalSuite extends munit.FunSuite {
  test("basics") {
    assertEquals(Set.from(run { for {
      (p, q, r) <- fresh[SExp, SExp, SExp]
      _ <- p =/= q && p =/= r && q =/= r
      _ <- evalExpo(p, (), q) && evalExpo(q, (), r) && evalExpo(r, (), p)
    } yield list(p, q, r): VarOr[SExp] }), Set("b"))
  }
}
