package littlejian.test

import littlejian.search.BFSimpPar
import littlejian.*
import littlejian.data.*
import littlejian.data.sexp.*
import littlejian.ext.*

class NumRelSuite extends munit.FunSuite {
  test("num") {
    assertEquals(Set.from(run[Int] { x => x < 9 && x > 1 }), Set("2", "3", "4", "5", "6", "7", "8"))
    assertEquals(Set.from(run[Int] { x => x < 9999 && x > 11 }), Set(
      """$1
11 < $1 < 9999"""))
    assertEquals(Set.from(run[Int] { x => x > 9 && x < 1 }), Set())
    assertEquals(Set.from(run[Int] { x => x + 1 === 10 }), Set("9"))
    assertEquals(Set.from(run[Int] { x => x + 1 === 10 && x > 1 }), Set("9"))
    assertEquals(Set.from(run[Int] { x => x - 1 === 10 && x > 1 }), Set("11"))
    assertEquals(Set.from(run[Int] { x => 2 - x === 10 }), Set("-8"))
    assertEquals(Set.from(run[Int] { x => x * 2 === 10 }), Set("5"))
    assertEquals(Set.from(run[Float] { x => x * 2 === 10 }), Set("5.0"))
  }

}
