package littlejian.examples.evalo.test

import littlejian.*
import littlejian.data.*
import littlejian.data.sexp.*
import littlejian.ext.*
import littlejian.unifier.*
import littlejian.search.BFSimp
import littlejian.examples.evalo._

class EvalSuite extends munit.FunSuite {
  val quineC = SExp.parse("((lambda (x)\n(list x (list (quote quote) x)))\n(quote\n(lambda (x)\n(list x (list (quote quote) x)))))")
  test("basics") {
    assertEquals(Set.from(run { lookupo(list(cons("a", "b")), "a") }), Set("b"))
    assertEquals(Set.from(run {
      lookupo(list(cons("a", "b"), cons("a", "c")), "a")
    }), Set("b"))
    assertEquals(Set.from(run {
      lookupo(list(cons("a", "b"), cons("a", "c")), "b")
    }), Set())
    assertEquals(Set.from(run {
      evalo(list(cons("a", "b"), cons("a", "c")), "a")
    }), Set("b"))
    assertEquals(Set.from(run {
      evalo(list(cons("a", "b"), cons("a", "c")), "b")
    }), Set())
    assertEquals(Set.from(run {
      val x: VarOr[SExp] = list("quote", "a")
      for {
        result <- fresh[SExp]
        _ <- x === list("quote", result)
      } yield result
    }), Set("a"))
    assertEquals(Set.from(run {
      evalo(list(), list("quote", "a"))
    }), Set("a"))
    assertEquals(Set.from(run {
      evalo(list(), list("quote", "a", "b"))
    }), Set())
    assertEquals(Set.from(run {
      evalo(list(), list("quote","a"))
    }), Set("a"))
    assertEquals(Set.from(run {
      evalo((), list("list", list("quote", "a")))
    }), Set("(a)"))
    assertEquals(Set.from(run {
      evalo((), list("car", list("list", list("quote", "a"))))
    }), Set("a"))
  }
  test("What eval to a") {
    assertEquals(run[SExp] { x => evalo((), x, "a") }.head, "(quote a)")
  }
  test("quineC"){
    assertEquals((run[SExp] { x => x === "Success" && evalo((), quineC, quineC) }).head,  "Success")
  }
  if(false) {
    test("quine3") {
      assertEquals(Set.from(run {
        for {
          (p, q, r) <- fresh[SExp, SExp, SExp]
          _ <- p =/= q && p =/= r && q =/= r
          _ <- evalo((), p, q) && evalo((), q, r) && evalo((), r, p)
        } yield list(p, q, r): VarOr[SExp]
      }), Set())
    }
  }
  test("apply"){
    assertEquals(Set.from(run {
      evalo((), list(list("lambda", list("x"), "x"), list("quote", "a")))
    }), Set("a"))
  }
  test("What eval to itself") {
    assertEquals(Set.from(run[SExp] { x => evalo((), x, x) }.take(1)), Set("(quote #1=(quote #1))"))
  }
}
