package littlejian.test

import littlejian.*
import littlejian.data.*
import littlejian.data.sexp.*
import littlejian.ext.*
import littlejian.search.naive.*
import littlejian.unifier.*


class TopSuite extends munit.FunSuite {
  test("example test that succeeds") {
    val obtained = 42
    val expected = 42
    assertEquals(obtained, expected)
  }
  test("basics") {
    assertEquals(Set.from(run { (x: VarOr[Int]) => x === 42 }), Set("42"))
    assertEquals(Set.from(run { (x: VarOr[Int]) => x === 42 && x === 32 }), Set())
    assertEquals(Set.from(run { (x: VarOr[Int]) => x === 42 || x === 32 }), Set("42","32"))
    assertEquals(Set.from(run { (x: VarOr[LList[Int]]) =>
      val head = hole[Int]
      val tail = hole[Int]
      x === LList(head, tail) && head === 42 && tail === 32
    }), Set("LCons(42,LCons(32,LEmpty()))"))
    assertEquals(Set.from(run (for {
      _ <- Rel.success
      x = hole[LList[Int]]
      head = hole[Int]
      tail = hole[Int]
      _ <- x === LList(head, tail)
      _ <- head === 42 && tail === 32
    } yield x)), Set("LCons(42,LCons(32,LEmpty()))"))
    assertEquals(Set.from(run[LList[Int]] { (x) =>
      val head = hole[Int]
      val tail = hole[Int]
      for {
        _ <- x === LList(head, tail)
        _ <- head === 42
        _ <- tail === 32
      } yield ()
    }), Set("LCons(42,LCons(32,LEmpty()))"))
    assertEquals(Set.from(run[Option[VarOr[Int]]] { x => {
      val i = hole[Int]
      i === 53 && x === Some(i)
    }}), Set("Some(53)"))
    assertEquals(Set.from(run[SExp] { x =>
      val a = hole[SExp]
      a === "a" && x === cons(a, a)
    }), Set("(a . a)"))
    assertEquals(Set.from(run {
      conde(1, 2, 3, 4) : Rel[Int]
    }), Set("1", "2", "3", "4"))
    implicit val U$IntStr: Unifier[Integer | String] = U$Union(U$Integer, U$String)
    assertEquals(Set.from(run[Integer | String] { x =>
      x.isType[Integer] && x === (1: Integer)
    }), Set("1"))
    assertEquals(Set.from(run[Integer | String] { x =>
      x.isType[Integer] && x === "a"
    }), Set())
    assertEquals(Set.from(run[Integer | String] { x =>
      x.isType[String] && x === "a"
    }), Set("a"))
    assertEquals(Set.from(run[Integer | String] { x =>
      x.isNotType[String] && x === (1: Integer)
    }), Set("1"))
    assertEquals(Set.from(run[SExp] { x =>
      x =/= "a" && x === "b"
    }), Set("b"))
    assertEquals(Set.from(run[SExp] { x =>
      x =/= "a" && x === "a"
    }), Set())
    assertEquals(Set.from(run[SExp] { x =>
      val y = hole[SExp]
      val z = hole[SExp]
      x =/= cons(y,z) && y === "a" && z === "b" && x === cons("a", "d")
    }), Set("(a . d)"))
    assertEquals(Set.from(run[SExp] { x =>
      val y = hole[SExp]
      val z = hole[SExp]
      x =/= cons(y, z) && y === "a" && z === "b" && x === cons("a", "b")
    }), Set())
  }
}
