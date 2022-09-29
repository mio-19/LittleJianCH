package littlejian.examples.evalo.test

import littlejian.*
import littlejian.data.*
import littlejian.data.sexp.*
import littlejian.ext.*
import littlejian.unifier.*
import littlejian.search.NaiveSearcher
import littlejian.examples.evalo._

class EvalSuite extends munit.FunSuite {
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
      val result = hole[SExp]
      for {
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
  test("apply"){
    assertEquals(Set.from(run {
      evalo((), list(list("lambda", list("x"), "x"), list("quote", "a")))
    }), Set("a"))
  }
  test("What eval to itself") {
    assertEquals(Set.from(run[SExp] { x => evalo((), x, x) }.take(1)), Set("(quote #1=(quote #1))"))
  }
}
