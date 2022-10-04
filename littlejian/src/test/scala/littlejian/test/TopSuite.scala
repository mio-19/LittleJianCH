package littlejian.test

import littlejian.*
import littlejian.data.*
import littlejian.data.sexp.*
import littlejian.ext.*
import littlejian.unifier.*


class TopSuite extends munit.FunSuite {
  runTest("NaiveSearcher ")(littlejian.search.NaiveSearcher)
  runTest("GradualSearcher ")(littlejian.search.deprecated.GradualSearcher)
  runTest("ReducingSearcher ")(littlejian.search.deprecated.ReducingSearcher)
  runTest("BFSimp ")(littlejian.search.BFSimp)
  runTest("BFSimpPar ")(littlejian.search.BFSimpPar)

  def runTest(name: String = "")(implicit searcher: Searcher): Unit = {
    test(name + "basics") {
      assertEquals(Set.from(run { (x: VarOr[Int]) => x === 42 }), Set("42"))
      assertEquals(Set.from(run { (x: VarOr[Int]) => x === 42 && x === 32 }), Set())
      assertEquals(Set.from(run { (x: VarOr[Int]) => x === 42 || x === 32 }), Set("42", "32"))
      assertEquals(Set.from(run { (x: VarOr[LList[Int]]) =>
        callWithFresh[Int] { head =>
          callWithFresh[Int] { tail =>
            x === LList(head, tail) && head === 42 && tail === 32
          }
        }
      }), Set("LCons(42,LCons(32,LEmpty()))"))
      assertEquals(Set.from(run(for {
        x <- fresh[LList[Int]]
        head <- fresh[Int]
        tail <- fresh[Int]
        _ <- x === LList(head, tail)
        _ <- head === 42 && tail === 32
      } yield x)), Set("LCons(42,LCons(32,LEmpty()))"))
      assertEquals(Set.from(run[LList[Int]] { (x) =>
        for {
          head <- fresh[Int]
          tail <- fresh[Int]
          _ <- x === LList(head, tail)
          _ <- head === 42
          _ <- tail === 32
        } yield ()
      }), Set("LCons(42,LCons(32,LEmpty()))"))
      assertEquals(Set.from(run[Option[VarOr[Int]]] { x => {
        for {
          i <- fresh[Int]
          _ <- i === 53 && x === Some(i)
        } yield ()
      }
      }), Set("Some(53)"))
      assertEquals(Set.from(run[SExp] { x =>
        for {
          a <- fresh[SExp]
          _ <- a === "a" && x === cons(a, a)
        } yield ()
      }), Set("(a . a)"))
      assertEquals(Set.from(run {
        conde(1, 2, 3, 4): Rel[Int]
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
        for {
          y <- fresh[SExp]
          z <- fresh[SExp]
          _ <- x =/= cons(y, z) && y === "a" && z === "b" && x === cons("a", "d")
        } yield ()
      }), Set("(a . d)"))
      assertEquals(Set.from(run[SExp] { x =>
        for {
          y <- fresh[SExp]
          z <- fresh[SExp]
          _ <- x =/= cons(y, z) && y === "a" && z === "b" && x === cons("a", "b")
        } yield ()
      }), Set())
      assertEquals(Set.from(run[SExp] {
        _ =/= "a"
      }), Set(
        """$1
$1 =/= a"""
      ))
    }
    test(name + "absent") {
      assertEquals(Set.from(run[SExp] { x =>
        x === cons("a", "a") && x.absent("a")
      }), Set())
      assertEquals(Set.from(run[SExp] { x =>
        x.absent("a") && x === cons("a", "a")
      }), Set())
      assertEquals(Set.from(run[SExp] { x =>
        x.absent("c") && x === cons("a", "a")
      }), Set("(a . a)"))
      assertEquals(Set.from(run[SExp] { x => {
        for {
          a <- fresh[SExp]
          _ <- x.absent("b") && x === cons(a, "a") && a === "b"
        } yield ()
      }
      }), Set())
      assertEquals(Set.from(run[SExp] { x => {
        for {
          a <- fresh[SExp]
          _ <- x.absent("c") && x === cons(a, "a") && a === "b"
        } yield ()
      }
      }), Set("(b . a)"))
    }
    test(name + "numbers") {
      assertEquals(Set.from(run[Int4] { x => x + Int4.from(4) === Int4.from(8) }), Set("4"))
      assertEquals(Set.from(run[Int4] { x => Int4.from(3) + Int4.from(4) === x }), Set("7"))
      assertEquals(Set.from(run[Int4] { x => Int4.from(7) - Int4.from(4) === x }), Set("3"))
      assertEquals(Set.from(run[Int4] { x => Int4.from(7) - Int4.from(8) === x }), Set("15")) // -1
      assertEquals(Set.from(run[Int4] { x => Int4.from(9) - Int4.from(8) === x }), Set("1"))
      assertEquals(Set.from(run[Int4] { x => x - Int4.from(8) === Int4.from(1) }), Set("9"))
      assertEquals(Set.from(run[Int4] { x => x - x === x }), Set("0"))
    }
  }

  def runMoreTest: Unit = {
    import littlejian.search.BFSimpPar
    test("numbers") {
      assertEquals(Set.from(run[Int8] { x => Int8.from(30) + Int8.from(4) === x }), Set("34"))
    }
  }
  //runMoreTest
}
