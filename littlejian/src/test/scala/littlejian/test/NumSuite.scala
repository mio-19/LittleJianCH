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
    assertEquals(Set.from(run[Int4] { x => Int4.from(9).minus(Int4.from(1)) === x }), Set("8"))
    assertEquals(Set.from(run[Int4] { x => Int4.from(9).-(Int4.from(1)) === x }), Set("8"))
    assertEquals(Set.from(run[Int16] { x => Int16.from(30) + Int16.from(4) === x }), Set("34"))
    assertEquals(Set.from(run[Int32] { x => Int32.from(30) + Int32.from(4) === x }), Set("34"))
    assertEquals(Set.from(run[Int16] { x => Int16.from(30) + x === Int16.from(33) }), Set("3"))
    assertEquals(Set.from(run[Int8] { x => Int8.from(9) - Int8.from(1) === x }), Set("8"))
    assertEquals(Set.from(run[Int4] {
      -(Int4.from(1): VarOr[Int4])
    }), Set("15"))
    assertEquals(Set.from(run[Int4] {
      !(Int4.from(1): VarOr[Int4])
    }), Set("14"))
    assertEquals(Set.from(run[Int8] {
      !(Int8.from(1): VarOr[Int8])
    }), Set("254"))
    assertEquals(Set.from(run[Int8] {
      Int8.from(254) + Int8.from(1)
    }), Set("255"))
    assertEquals(Set.from(run[Int8] { - (Int8.from(1): VarOr[Int8]) }), Set("255"))
    assertEquals(Set.from(run[Int8] { x => Int8.from(9).minus(Int8.from(1)) === x }), Set("8"))
    assertEquals(Set.from(run[Int16] { x => Int16.from(9).minus(Int16.from(1)) === x }), Set("8"))
    assertEquals(Set.from(run[Int16] { x => Int16.from(9).-(Int16.from(1)) === x }), Set("8"))
    assertEquals(Set.from(run[Int32] { x => Int32.from(65598).-(Int32.from(97)) === x }), Set("65501"))
    assertEquals(Set.from(run[Nat] { x => Nat.from(9) + Nat.from(1) === x}), Set("10"))
    assertEquals(Set.from(run[Nat] { x => Nat.from(9) - Nat.from(1) === x }), Set("8"))
    assertEquals(Set.from(run[Nat] { x => Nat.from(9) * Nat.from(16) === x }), Set("144"))
    assertEquals(Set.from(run[BinaryNat] { x => BinaryNat.from(9) + BinaryNat.from(1) === x }), Set("10"))
    assertEquals(Set.from(run[BinaryNat] { x => BinaryNat.from(9) - BinaryNat.from(1) === x }), Set("8"))
    assertEquals(Set.from(run[BinaryNat] { x => BinaryNat.from(65598) + BinaryNat.from(768) === x }), Set("66366"))
    assertEquals(Set.from(run[BinaryNat] { BinaryNat.from(9).prev }), Set("8"))
    if(false){
      assertEquals(Set.from(run[BinaryNat] {
        BinaryNat.from(9) * BinaryNat.from(2)
      }), Set("18"))
    }
  }
  test("solve") {
    assertEquals(Set.from(run[Int8] { x => x + x === x }), Set("0"))
    assertEquals(Set.from(run[Int8] { x => x - x === x }), Set("0"))
  }
  test("solveSlow"){
    assertEquals(Set.from(run[Int16] { x => x + x === x + Int16.from(1) }), Set("1"))
  }
}