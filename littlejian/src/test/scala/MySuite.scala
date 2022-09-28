import littlejian._
import littlejian.ext._
import littlejian.search.naive._
import littlejian.data._


class MySuite extends munit.FunSuite {
  test("example test that succeeds") {
    val obtained = 42
    val expected = 42
    assertEquals(obtained, expected)
  }
  test("basics") {
    assertEquals(run { (x: VarOr[Int]) => x === 42 }, "42\n")
    assertEquals(run { (x: VarOr[Int]) => x === 42 && x === 32 }, "")
    assertEquals(run { (x: VarOr[Int]) => x === 42 || x === 32 }, "42\n32\n")
    assertEquals(run { (x: VarOr[LList[Int]]) =>
      val head = hole[Int]
      val tail = hole[Int]
      x === LList(head, tail) && head === 42 && tail === 32
    }, "LCons(42,LCons(32,LEmpty()))\n")
  }
}
