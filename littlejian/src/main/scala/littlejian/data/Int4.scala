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

implicit class VarOrInt4Ops(self: VarOr[Int4]) {
  def get: GoalWith[Int4] = for {
    x <- self.is[Boolean, Boolean, Boolean, Boolean](Int4(_, _, _, _))
  } yield Int4(x._1, x._2, x._3, x._4)

  def +(other: VarOr[Int4]): Rel[Int4] = for {
    x <- self.get
    y <- other.get
    (c, r) <- x.plus(y)
  } yield r

  def unary_- : Rel[Int4] = for {
    x <- self.get
    r <- -x
  } yield r

  def -(other: VarOr[Int4]): Rel[Int4] = for {
    y <- -other
    r <- self + y
  } yield r
}

implicit val U$Int4: Unifier[Int4] = U$Product

object Int4 {
  def zero: Int4 = Int4(false, false, false, false)

  def one: Int4 = Int4(true, false, false, false)

  def from(x: Int): Int4 = {
    if (x < 0 || x > 15) throw new IllegalArgumentException("x must be in [0, 15]")
    Int4((x & 1) == 1, (x & 2) == 2, (x & 4) == 4, (x & 8) == 8)
  }
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
    val lo = Int4.from(x & 15)
    val hi = Int4.from((x >> 4) & 15)
    Int8(lo, hi)
  }
}

implicit class VarOrInt8Ops(self: VarOr[Int8]) {
  def get: GoalWith[Int8] = for {
    (b0, b1, b2, b3) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b4, b5, b6, b7) <- fresh[Boolean, Boolean, Boolean, Boolean]
    result = Int8(Int4(b0, b1, b2, b3), Int4(b4, b5, b6, b7))
    _ <- self == result
  } yield result

  def +(other: VarOr[Int8]): Rel[Int8] = for {
    x <- self.get
    y <- other.get
    (c, r) <- x.plus(x)
  } yield r

  def unary_- : Rel[Int8] = for {
    x <- self.get
    r <- -x
  } yield r

  def -(other: VarOr[Int8]): Rel[Int8] = for {
    y <- -other
    r <- self + y
  } yield r
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

implicit class VarOrInt16Ops(self: VarOr[Int16]) {
  def get: GoalWith[Int16] = for {
    (b0, b1, b2, b3) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b4, b5, b6, b7) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b8, b9, b10, b11) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b12, b13, b14, b15) <- fresh[Boolean, Boolean, Boolean, Boolean]
    result = Int16(Int8(Int4(b0, b1, b2, b3), Int4(b4, b5, b6, b7)), Int8(Int4(b8, b9, b10, b11), Int4(b12, b13, b14, b15)))
    _ <- self == result
  } yield result

  def +(other: VarOr[Int16]): Rel[Int16] = for {
    x <- self.get
    y <- other.get
    (c, r) <- x.plus(x)
  } yield r

  def unary_- : Rel[Int16] = for {
    x <- self.get
    r <- -x
  } yield r

  def -(other: VarOr[Int16]): Rel[Int16] = for {
    y <- -other
    r <- self + y
  } yield r
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

implicit class VarOrInt32Ops(self: VarOr[Int32]) {
  def get: GoalWith[Int32] = for {
    (b0, b1, b2, b3) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b4, b5, b6, b7) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b8, b9, b10, b11) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b12, b13, b14, b15) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b16, b17, b18, b19) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b20, b21, b22, b23) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b24, b25, b26, b27) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b28, b29, b30, b31) <- fresh[Boolean, Boolean, Boolean, Boolean]
    result = Int32(Int16(Int8(Int4(b0, b1, b2, b3), Int4(b4, b5, b6, b7)), Int8(Int4(b8, b9, b10, b11), Int4(b12, b13, b14, b15))), Int16(Int8(Int4(b16, b17, b18, b19), Int4(b20, b21, b22, b23)), Int8(Int4(b24, b25, b26, b27), Int4(b28, b29, b30, b31))))
    _ <- self == result
  } yield result

  def +(other: VarOr[Int32]): Rel[Int32] = for {
    x <- self.get
    y <- other.get
    (c, r) <- x.plus(x)
  } yield r

  def unary_- : Rel[Int32] = for {
    x <- self.get
    r <- -x
  } yield r

  def -(other: VarOr[Int32]): Rel[Int32] = for {
    y <- -other
    r <- self + y
  } yield r
}

object Int32 {
  def from(n: Int): Int32 = {
    val lo = Int16.from((n & 0xffff).toShort)
    val hi = Int16.from(((n >> 16) & 0xffff).toShort)
    Int32(lo, hi)
  }
}


type BinaryNatVal = LList[Boolean]

final case class BinaryNat(xs: VarOr[LList[Boolean]]) extends Product1[VarOr[LList[Boolean]]] {
  def succ: Rel[BinaryNatVal] = conde(
    xs.eqEmpty >> LList(true),
    for {
      (xh, xt) <- xs.is[Boolean, LList[Boolean]](LCons(_, _))
      (c, r) <- add(xh, true)
      rest <- c.switch(BinaryNat(xt).succ, xt)
    } yield LCons(r, rest)
  )

  def plus(that: VarOr[BinaryNatVal]): Rel[BinaryNatVal] = conde(
    xs.eqEmpty >> that,
    that.eqEmpty >> xs,
    for {
      (xh, xt) <- xs.is[Boolean, LList[Boolean]](LCons(_, _))
      (yh, yt) <- that.is[Boolean, LList[Boolean]](LCons(_, _))
      (c, r) <- add(xh, yh)
      rest <- BinaryNat(xt).plus(yt, c)
    } yield LCons(r, rest)
  )

  def plus(that: VarOr[BinaryNatVal], c: VarOr[Boolean]): Rel[BinaryNatVal] = c.switch(
    conde(
      xs.eqEmpty >> BinaryNat(that).succ,
      that.eqEmpty >> this.succ,
      for {
        (xh, xt) <- xs.is[Boolean, LList[Boolean]](LCons(_, _))
        (yh, yt) <- that.is[Boolean, LList[Boolean]](LCons(_, _))
        (c, r) <- add(xh, yh, true)
        rest <- BinaryNat(xt).plus(yt, c)
      } yield LCons(r, rest)
    ),
    this.plus(that)
  )
}

object BinaryNat {
  def zero = from(0)

  def one = from(1)

  def from(n: Int): BinaryNat =
    if (n < 0) throw new IllegalArgumentException("n must be non-negative")
    else if (n == 0) BinaryNat(LList.empty)
    else {
      val lo = (n & 1) == 1
      val hi = BinaryNat.from(n >> 1).xs
      BinaryNat(LCons(lo, hi))
    }
}

implicit class VarOrBinaryNatOps(self: VarOr[BinaryNat]) {
  def succ: Rel[BinaryNat] = for {
    xs <- self.is[BinaryNatVal](BinaryNat(_))
    result <- BinaryNat(xs).succ
  } yield BinaryNat(result)

  def +(other: VarOr[BinaryNat]): Rel[BinaryNat] = for {
    xs <- self.is[BinaryNatVal](BinaryNat(_))
    ys <- other.is[BinaryNatVal](BinaryNat(_))
    result <- BinaryNat(xs).plus(ys)
  } yield BinaryNat(result)

  def -(other: VarOr[BinaryNat]): Rel[BinaryNat] = for {
    result <- fresh[BinaryNat]
    _ <- other + result === self
  } yield result
}

implicit val U$BinaryNat: Unifier[BinaryNat] = U$Product(U$VarOr(U$LList(U$Boolean)))

final case class BinaryInt(sign: VarOr[Boolean], x: BinaryNat) extends Product2[VarOr[Boolean], BinaryNat]

object BinaryInt {
  def from(n: BinaryNat): BinaryInt = BinaryInt(true, n)

  def from(n: Int): BinaryInt = if (n >= 0) from(BinaryNat.from(n)) else BinaryInt(false, BinaryNat.from((-n) - 1))
}

implicit val U$BinaryInt: Unifier[BinaryInt] = U$Product