import littlejian._
import littlejian.ext._
import littlejian.search.naive._


class MySuite extends munit.FunSuite {
  test("example test that succeeds") {
    val obtained = 42
    val expected = 42
    assertEquals(obtained, expected)
  }
  test("basics") {
    assertEquals(run { (x: VarOr[Int]) => x === 42 }, "42\n")
    assertEquals(run { (x: VarOr[Int]) => x === 42 && x === 32 }, "")
  }
}
