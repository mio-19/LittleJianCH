package littlejian.data

import littlejian.*
import littlejian.ext.*

import scala.language.implicitConversions

// in this package, Int*.toString is unsigned, BinaryInt.toString is signed

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


trait IntN[T <: IntN[T]] {
  def plus(that: T): GoalWith[(VarOr[Boolean], T)]

  def plus(that: T, carry: VarOr[Boolean]): GoalWith[(VarOr[Boolean], T)]

  def succ: GoalWith[(VarOr[Boolean], T)]

  def unary_! : GoalWith[T]

  def unary_- : GoalWith[T] = for {
    n <- !this
    (c, r) <- n.succ
  } yield r

  def bits: Vector[VarOr[Boolean]]

  override def toString: String = {
    val bs = bits.map(_.toString)
    if (bs.forall(x => x == "true" || x == "false")) {
      val bs0: Vector[Int] = bs.map(x => if (x == "true") 1 else 0)
      val result: Int = bs0.zipWithIndex.map(x => x._1 << x._2).sum
      result.toString
    } else {
      s"${this.getClass.getSimpleName}(${bs.mkString(" , ")})"
    }
  }
}

final case class Int4(bit0: VarOr[Boolean], bit1: VarOr[Boolean], bit2: VarOr[Boolean], bit3: VarOr[Boolean]) extends IntN[Int4] derives Unifier {
  override def plus(that: Int4): GoalWith[(VarOr[Boolean], Int4)] = for {
    (c0, r0) <- add(bit0, that.bit0)
    (c1, r1) <- add(bit1, that.bit1, c0)
    (c2, r2) <- add(bit2, that.bit2, c1)
    (c3, r3) <- add(bit3, that.bit3, c2)
  } yield (c3, Int4(r0, r1, r2, r3))

  override def plus(that: Int4, carry: VarOr[Boolean]): GoalWith[(VarOr[Boolean], Int4)] = for {
    (c0, r0) <- add(bit0, that.bit0, carry)
    (c1, r1) <- add(bit1, that.bit1, c0)
    (c2, r2) <- add(bit2, that.bit2, c1)
    (c3, r3) <- add(bit3, that.bit3, c2)
  } yield (c3, Int4(r0, r1, r2, r3))

  override def succ: GoalWith[(VarOr[Boolean], Int4)] = plus(Int4(true, false, false, false))

  override def unary_! : GoalWith[Int4] = for {
    b0 <- !bit0
    b1 <- !bit1
    b2 <- !bit2
    b3 <- !bit3
  } yield Int4(b0, b1, b2, b3)

  override def bits: Vector[VarOr[Boolean]] = Vector(bit0, bit1, bit2, bit3)
}

abstract class VarOrIntNOps[T <: IntN[T]](self: VarOr[T], unifier: Unifier[T]) {
  private implicit val v: Unifier[T] = unifier

  protected def consThis(x: VarOr[T]): VarOrIntNOps[T]

  def get: GoalWith[T]

  final inline def +(other: VarOr[T]): Rel[T] = for {
    x <- this.get
    y <- consThis(other).get
    (c, r) <- x.plus(y)
  } yield r

  final inline def unary_! : Rel[T] = for {
    x <- this.get
    r <- !x
  } yield r

  final inline def unary_- : Rel[T] = for {
    x <- this.get
    r <- -x
  } yield r

  // Alternative implementation
  final inline def minus(other: VarOr[T]): Rel[T] = for {
    y <- -consThis(other)
    r <- this + y
  } yield r

  final inline def -(other: VarOr[T]): Rel[T] = for {
    result <- fresh[T]
    _ <- consThis(other) + result === self
  } yield result
}

implicit class VarOrInt4Ops(self: VarOr[Int4]) extends VarOrIntNOps[Int4](self, implicitly[Unifier[Int4]]) {
  override protected def consThis(x: VarOr[Int4]): VarOrInt4Ops = VarOrInt4Ops(x)

  override def get: GoalWith[Int4] = for {
    x <- self.is[Boolean, Boolean, Boolean, Boolean](Int4(_, _, _, _))
  } yield Int4(x._1, x._2, x._3, x._4)
}

object Int4 {
  def zero: Int4 = Int4(false, false, false, false)

  def one: Int4 = Int4(true, false, false, false)

  def from(x: Int): Int4 = {
    if (x < 0 || x > 15) throw new IllegalArgumentException("x must be in [0, 15]")
    Int4((x & 1) == 1, (x & 2) == 2, (x & 4) == 4, (x & 8) == 8)
  }
}


final case class Int8(lo: Int4, hi: Int4) extends IntN[Int8] derives Unifier {
  override def plus(that: Int8): GoalWith[(VarOr[Boolean], Int8)] = for {
    (c, r) <- lo.plus(that.lo)
    (c2, r2) <- hi.plus(that.hi, c)
  } yield (c2, Int8(r, r2))

  override def plus(that: Int8, carry: VarOr[Boolean]): GoalWith[(VarOr[Boolean], Int8)] = for {
    (c, r) <- lo.plus(that.lo, carry)
    (c2, r2) <- hi.plus(that.hi, c)
  } yield (c2, Int8(r, r2))

  override inline def succ: GoalWith[(VarOr[Boolean], Int8)] = plus(Int8.one)

  override inline def unary_! : GoalWith[Int8] = for {
    l <- !lo
    h <- !hi
  } yield Int8(l, h)

  override inline def bits: Vector[VarOr[Boolean]] = lo.bits ++ hi.bits
}

object Int8 {
  def zero: Int8 = from(0)

  def one: Int8 = from(1)

  def from(x: Byte): Int8 = {
    val lo = Int4.from(x & 15)
    val hi = Int4.from((x >>> 4) & 15)
    Int8(lo, hi)
  }

  def from(x: Int): Int8 = {
    if (x < 0 || x > 255) throw new IllegalArgumentException("x must be in [0, 255]")
    val lo = Int4.from(x & 15)
    val hi = Int4.from((x >>> 4) & 15)
    Int8(lo, hi)
  }
}

implicit class VarOrInt8Ops(self: VarOr[Int8])(using u: Unifier[Int8]) extends VarOrIntNOps[Int8](self, u) {
  override protected def consThis(x: VarOr[Int8]): VarOrInt8Ops = VarOrInt8Ops(x)

  def get: GoalWith[Int8] = for {
    (b0, b1, b2, b3) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b4, b5, b6, b7) <- fresh[Boolean, Boolean, Boolean, Boolean]
    result = Int8(Int4(b0, b1, b2, b3), Int4(b4, b5, b6, b7))
    _ <- self === result
  } yield result
}

final case class Int16(lo: Int8, hi: Int8) extends IntN[Int16] derives Unifier {
  override def plus(that: Int16): GoalWith[(VarOr[Boolean], Int16)] = for {
    (c, r) <- lo.plus(that.lo)
    (c2, r2) <- hi.plus(that.hi, c)
  } yield (c2, Int16(r, r2))

  override def plus(that: Int16, carry: VarOr[Boolean]): GoalWith[(VarOr[Boolean], Int16)] = for {
    (c, r) <- lo.plus(that.lo, carry)
    (c2, r2) <- hi.plus(that.hi, c)
  } yield (c2, Int16(r, r2))

  override inline def succ: GoalWith[(VarOr[Boolean], Int16)] = plus(Int16.one)

  override inline def unary_! : GoalWith[Int16] = for {
    l <- !lo
    h <- !hi
  } yield Int16(l, h)

  override inline def bits: Vector[VarOr[Boolean]] = lo.bits ++ hi.bits
}

implicit class VarOrInt16Ops(self: VarOr[Int16])(using u: Unifier[Int16]) extends VarOrIntNOps[Int16](self, u) {
  override protected def consThis(x: VarOr[Int16]): VarOrIntNOps[Int16] = VarOrInt16Ops(x)

  override def get: GoalWith[Int16] = for {
    (b0, b1, b2, b3) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b4, b5, b6, b7) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b8, b9, b10, b11) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b12, b13, b14, b15) <- fresh[Boolean, Boolean, Boolean, Boolean]
    result = Int16(Int8(Int4(b0, b1, b2, b3), Int4(b4, b5, b6, b7)), Int8(Int4(b8, b9, b10, b11), Int4(b12, b13, b14, b15)))
    _ <- self === result
  } yield result
}

object Int16 {
  def zero = from(0)

  def one = from(1)

  def from(x: Short): Int16 = {
    val lo = Int8.from((x & 0xff).toByte)
    val hi = Int8.from(((x >>> 8) & 0xff).toByte)
    Int16(lo, hi)
  }
}

final case class Int32(lo: Int16, hi: Int16) extends IntN[Int32] derives Unifier {
  override def plus(that: Int32): GoalWith[(VarOr[Boolean], Int32)] = for {
    (c, r) <- lo.plus(that.lo)
    (c2, r2) <- hi.plus(that.hi, c)
  } yield (c2, Int32(r, r2))

  override def plus(that: Int32, carry: VarOr[Boolean]): GoalWith[(VarOr[Boolean], Int32)] = for {
    (c, r) <- lo.plus(that.lo, carry)
    (c2, r2) <- hi.plus(that.hi, c)
  } yield (c2, Int32(r, r2))

  override inline def succ: GoalWith[(VarOr[Boolean], Int32)] = plus(Int32.one)

  override inline def unary_! : GoalWith[Int32] = for {
    l <- !lo
    h <- !hi
  } yield Int32(l, h)

  override inline def bits: Vector[VarOr[Boolean]] = lo.bits ++ hi.bits
}

implicit class VarOrInt32Ops(self: VarOr[Int32])(using u: Unifier[Int32]) extends VarOrIntNOps[Int32](self, u) {
  override protected def consThis(x: VarOr[Int32]): VarOrIntNOps[Int32] = VarOrInt32Ops(x)

  override def get: GoalWith[Int32] = for {
    (b0, b1, b2, b3) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b4, b5, b6, b7) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b8, b9, b10, b11) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b12, b13, b14, b15) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b16, b17, b18, b19) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b20, b21, b22, b23) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b24, b25, b26, b27) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b28, b29, b30, b31) <- fresh[Boolean, Boolean, Boolean, Boolean]
    result = Int32(Int16(Int8(Int4(b0, b1, b2, b3), Int4(b4, b5, b6, b7)), Int8(Int4(b8, b9, b10, b11), Int4(b12, b13, b14, b15))), Int16(Int8(Int4(b16, b17, b18, b19), Int4(b20, b21, b22, b23)), Int8(Int4(b24, b25, b26, b27), Int4(b28, b29, b30, b31))))
    _ <- self === result
  } yield result
}

object Int32 {
  def zero = from(0)

  def one = from(1)

  def from(n: Int): Int32 = {
    val lo = Int16.from((n & 0xffff).toShort)
    val hi = Int16.from(((n >>> 16) & 0xffff).toShort)
    Int32(lo, hi)
  }
}


type BinaryNatVal = LList[Boolean]

final case class BinaryNat(xs: VarOr[LList[Boolean]]) extends Product1[VarOr[LList[Boolean]]] derives Unifier {
  def succ: Rel[BinaryNatVal] = conde(
    xs.eqEmpty >> LList(true),
    for {
      (xh, xt) <- xs.is[Boolean, LList[Boolean]](LCons(_, _))
      (c, r) <- add(xh, true)
      rest <- c.elim{BinaryNat(xt).succ} {xt}
    } yield LCons(r, rest)
  )

  def prev0: Rel[BinaryNatVal] = for {
    result <- fresh[BinaryNatVal]
    a <- BinaryNat(result).succ
    _ <- a === xs
  } yield result

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

  def plus(that: VarOr[BinaryNatVal], c: VarOr[Boolean]): Rel[BinaryNatVal] = c.elim{
    conde(
      xs.eqEmpty >> BinaryNat(that).succ,
      that.eqEmpty >> this.succ,
      for {
        (xh, xt) <- xs.is[Boolean, LList[Boolean]](LCons(_, _))
        (yh, yt) <- that.is[Boolean, LList[Boolean]](LCons(_, _))
        (c, r) <- add(xh, yh, true)
        rest <- BinaryNat(xt).plus(yt, c)
      } yield LCons(r, rest)
    )
  } {
    this.plus(that)
  }

  def mul(that: VarOr[BinaryNatVal]): Rel[BinaryNatVal] = conde(
    that.eqEmpty >> BinaryNat.zero.xs,
    for {
      that0 <- BinaryNat(that).prev0
      result0 <- this.mul(that0)
      result <- BinaryNat(result0).plus(xs)
    } yield result
  )

  override def toString: String = xs.getStrings match {
    case s: String => s"BinaryNat($s)"
    case (s, xs) => bitsToNat(xs, s"BinaryNat($s)")
  }
}

inline private def bitsToNat(xs: Vector[String], default: => String): String = try {
  val bits = xs.map({
    case "true" => true
    case "false" => false
    case _ => throw new UnsupportedOperationException()
  })
  val n = bits.zipWithIndex.map { case (b, i) => if (b) 1 << i else 0 }.sum
  n.toString
} catch {
  case _: UnsupportedOperationException => default
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

implicit class VarOrBinaryNatOps(self: VarOr[BinaryNat])(using u: Unifier[BinaryNat]) {
  def succ: Rel[BinaryNat] = for {
    xs <- self.is[BinaryNatVal](BinaryNat(_))
    result <- BinaryNat(xs).succ
  } yield BinaryNat(result)

  def prev: Rel[BinaryNat] = for {
    xs <- self.is[BinaryNatVal](BinaryNat(_))
    result <- BinaryNat(xs).prev0
  } yield BinaryNat(result)

  def +(other: VarOr[BinaryNat]): Rel[BinaryNat] = for {
    xs <- self.is[BinaryNatVal](BinaryNat(_))
    ys <- other.is[BinaryNatVal](BinaryNat(_))
    result <- BinaryNat(xs).plus(ys)
  } yield BinaryNat(result)

  def *(other: VarOr[BinaryNat]): Rel[BinaryNat] = for {
    xs <- self.is[BinaryNatVal](BinaryNat(_))
    ys <- other.is[BinaryNatVal](BinaryNat(_))
    result <- BinaryNat(xs).mul(ys)
  } yield BinaryNat(result)

  def -(other: VarOr[BinaryNat]): Rel[BinaryNat] = for {
    result <- fresh[BinaryNat]
    _ <- other + result === self
  } yield result
}

final case class BinaryInt(sign: VarOr[Boolean], x: BinaryNat) extends Product2[VarOr[Boolean], BinaryNat] derives Unifier

object BinaryInt {
  def from(n: BinaryNat): BinaryInt = BinaryInt(true, n)

  def from(n: Int): BinaryInt = if (n >= 0) from(BinaryNat.from(n)) else BinaryInt(false, BinaryNat.from((-n) - 1))
}

// nat with fixed size
final case class FixedNat(xs: VarOr[LList[Boolean]]) {
  override def toString: String = xs.getStrings match {
    case s: String => s"FixedNat($s)"
    case (s, xs) => bitsToNat(xs, s"FixedNat($s)")
  }
}