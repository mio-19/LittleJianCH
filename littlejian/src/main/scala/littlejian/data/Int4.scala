package littlejian.data

import littlejian.*
import littlejian.ext._

def add(x: VarOr[Boolean], y: VarOr[Boolean]): GoalWith[(VarOr[Boolean], VarOr[Boolean])] = for {
  c <- fresh[Boolean]
  r <- fresh[Boolean]
  _ <- conde(
    begin(x === true, y === true, c === true, r === false), // 1+1=10
    begin(x === true, y === false, c === false, r === true), // 1+0=01
    begin(x === false, y === true, c === false, r === true), // 0+1=01
    begin(x === false, y === false, c === false, r === false) // 0+0=00
  )
} yield (c, r)

def add(x: VarOr[Boolean], y: VarOr[Boolean], z: VarOr[Boolean]): GoalWith[(VarOr[Boolean], VarOr[Boolean])] = for {
  c <- fresh[Boolean]
  r <- fresh[Boolean]
  _ <- conde(
    begin(x === true, y === true, z === true, c === true, r === true), // 1+1+1=11
    begin(x === true, y === true, z === false, c === true, r === false), // 1+1+0=10
    begin(x === true, y === false, z === true, c === true, r === false), // 1+0+1=10
    begin(x === true, y === false, z === false, c === false, r === true), // 1+0+0=01
    begin(x === false, y === true, z === true, c === true, r === false), // 0+1+1=10
    begin(x === false, y === true, z === false, c === false, r === true), // 0+1+0=01
    begin(x === false, y === false, z === true, c === false, r === true), // 0+0+1=01
    begin(x === false, y === false, z === false, c === false, r === false) // 0+0+0=00
  )
} yield (c, r)


final case class Int4(bit0: VarOr[Boolean], bit1: VarOr[Boolean], bit2: VarOr[Boolean], bit3: VarOr[Boolean]) extends Product4[VarOr[Boolean], VarOr[Boolean], VarOr[Boolean], VarOr[Boolean]] {
  def plus(that: Int4): GoalWith[(VarOr[Boolean], Int4)] = for {
    (c0, r0) <- add(bit0, that.bit0)
    (c1, r1) <- add(bit1, that.bit1, c0)
    (c2, r2) <- add(bit2, that.bit2, c1)
    (c3, r3) <- add(bit3, that.bit3, c2)
  } yield (c3, Int4(r0, r1, r2, r3))

  def plus(that: Int4, carry: VarOr[Boolean]): GoalWith[(VarOr[Boolean], Int4)] = for {
    (c0, r0) <- add(bit0, that.bit0, carry)
    (c1, r1) <- add(bit1, that.bit1, c0)
    (c2, r2) <- add(bit2, that.bit2, c1)
    (c3, r3) <- add(bit3, that.bit3, c2)
  } yield (c3, Int4(r0, r1, r2, r3))

  def succ: GoalWith[(VarOr[Boolean], Int4)] = plus(Int4(true, false, false, false))

  def unary_! : GoalWith[Int4] = for {
    b0 <- !bit0
    b1 <- !bit1
    b2 <- !bit2
    b3 <- !bit3
  } yield Int4(b0, b1, b2, b3)

  def unary_- : GoalWith[Int4] = for {
    n <- !this
    (c, r) <- n.succ
  } yield r
}

implicit val U$Int4: Unifier[Int4] = U$Product

object Int4 {
  def zero: Int4 = Int4(false, false, false, false)

  def one: Int4 = Int4(true, false, false, false)
}


final case class Int8(lo: Int4, hi: Int4) extends Product2[Int4, Int4] {
  def plus(that: Int8): GoalWith[(VarOr[Boolean], Int8)] = for {
    (c, r) <- lo.plus(that.lo)
    (c2, r2) <- hi.plus(that.hi, c)
  } yield (c2, Int8(r, r2))

  def plus(that: Int8, carry: VarOr[Boolean]): GoalWith[(VarOr[Boolean], Int8)] = for {
    (c, r) <- lo.plus(that.lo, carry)
    (c2, r2) <- hi.plus(that.hi, c)
  } yield (c2, Int8(r, r2))

  def succ: GoalWith[(VarOr[Boolean], Int8)] = plus(Int8.one)

  def unary_! : GoalWith[Int8] = for {
    l <- !lo
    h <- !hi
  } yield Int8(l, h)

  def unary_- : GoalWith[Int8] = for {
    n <- !this
    (c, r) <- n.succ
  } yield r
}

object Int8 {
  def zero: Int8 = Int8(Int4.zero, Int4.zero)

  def one: Int8 = Int8(Int4.one, Int4.one)

  def from(x: Byte): Int8 = {
    val lo = Int4((x & 1) == 1, (x & 2) == 2, (x & 4) == 4, (x & 8) == 8)
    val hi = Int4((x & 16) == 16, (x & 32) == 32, (x & 64) == 64, (x & 128) == 128)
    Int8(lo, hi)
  }
}


implicit val U$Int8: Unifier[Int8] = U$Product

final case class Int16(lo: Int8, hi: Int8) extends Product2[Int8, Int8] {
  def plus(that: Int16): GoalWith[(VarOr[Boolean], Int16)] = for {
    (c, r) <- lo.plus(that.lo)
    (c2, r2) <- hi.plus(that.hi, c)
  } yield (c2, Int16(r, r2))

  def plus(that: Int16, carry: VarOr[Boolean]): GoalWith[(VarOr[Boolean], Int16)] = for {
    (c, r) <- lo.plus(that.lo, carry)
    (c2, r2) <- hi.plus(that.hi, c)
  } yield (c2, Int16(r, r2))
}

implicit val U$Int16: Unifier[Int16] = U$Product

object Int16 {
  def from(x: Short): Int16 = {
    val lo = Int8.from((x & 0xff).toByte)
    val hi = Int8.from(((x >> 8) & 0xff).toByte)
    Int16(lo, hi)
  }
}

final case class Int32(lo: Int16, hi: Int16) extends Product2[Int16, Int16] {
  def plus(that: Int32): GoalWith[(VarOr[Boolean], Int32)] = for {
    (c, r) <- lo.plus(that.lo)
    (c2, r2) <- hi.plus(that.hi, c)
  } yield (c2, Int32(r, r2))

  def plus(that: Int32, carry: VarOr[Boolean]): GoalWith[(VarOr[Boolean], Int32)] = for {
    (c, r) <- lo.plus(that.lo, carry)
    (c2, r2) <- hi.plus(that.hi, c)
  } yield (c2, Int32(r, r2))
}

object Int32 {
  def from(n: Int): Int32 = {
    val lo = Int16.from((n & 0xffff).toShort)
    val hi = Int16.from(((n >> 16) & 0xffff).toShort)
    Int32(lo, hi)
  }
}
