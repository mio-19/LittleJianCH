package littlejian.test

import littlejian.search.BFSimpPar
import littlejian.*
import littlejian.data.*
import littlejian.data.sexp.*
import littlejian.ext.*
import littlejian.unifier.*


class NumSuite extends munit.FunSuite {
  test("numbers") {
    assertEquals(Set.from(run[Int8] { x => Int8.from(30) + Int8.from(4) === x }), Set("34"))
  }
}