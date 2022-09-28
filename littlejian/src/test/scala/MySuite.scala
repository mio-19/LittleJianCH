import littlejian._
import littlejian.ext._
import littlejian.search.naive._
import littlejian.data._
import littlejian.unifier._
import littlejian.data.sexp._


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
    assertEquals(run (for {
      _ <- Rel.success
      x = hole[LList[Int]]
      head = hole[Int]
      tail = hole[Int]
      _ <- x === LList(head, tail)
      _ <- head === 42 && tail === 32
    } yield x), "LCons(42,LCons(32,LEmpty()))\n")
    assertEquals(run[LList[Int]] { (x) =>
      val head = hole[Int]
      val tail = hole[Int]
      for {
        _ <- x === LList(head, tail)
        _ <- head === 42
        _ <- tail === 32
      } yield ()
    }, "LCons(42,LCons(32,LEmpty()))\n")
    assertEquals(run[Option[VarOr[Int]]] { x => {
      val i = hole[Int]
      i === 53 && x === Some(i)
    }}, "Some(53)\n")
    assertEquals(run[SExp] { x =>
      val a = hole[SExp]
      a === "a" && x === cons(a, a)
    }, "(a . a)\n")
    assertEquals(run {
      conde(1, 2, 3, 4) : Rel[Int]
    }, "1\n3\n2\n4\n")
  }
}
