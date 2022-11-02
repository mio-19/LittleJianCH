package littlejian.test

import littlejian.search.BFSimpPar
import littlejian.*
import littlejian.data.*
import littlejian.data.sexp.*
import littlejian.ext.*

class NumRelSuite extends munit.FunSuite {
  test("num") {
    assertEquals(Set.from(run[Int] { x => x < 9 && x > 1 }), Set(""))
  }

}
