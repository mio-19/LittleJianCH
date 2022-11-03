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

final case class Int4(bit0: VarOr[Boolean], bit1: VarOr[Boolean], bit2: VarOr[Boolean], bit3: VarOr[Boolean]) extends IntN[Int4] derives Unify {
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

abstract class VarOrIntNOps[T <: IntN[T]](self: VarOr[T], unifier: Unify[T]) {
  private implicit val v: Unify[T] = unifier

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

  final inline def elim[U](clauses: (VarOr[T], Rel[U])*)(implicit u: Unify[U]): Rel[U] =
    conde(clauses.map(x => (x._1 === self) >> x._2) *)
}

implicit class VarOrInt4Ops(self: VarOr[Int4]) extends VarOrIntNOps[Int4](self, implicitly[Unify[Int4]]) {
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

  def from(xs: Vector[VarOr[Boolean]]): Int4 = xs match {
    case Vector(b1, b2, b3, b4) => Int4(b1, b2, b3, b4)
    case _ => throw new IllegalArgumentException("xs must have 4 elements")
  }
}

// aka Byte
final case class Int8(lo: Int4, hi: Int4) extends IntN[Int8] derives Unify {
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

  def from(xs: Vector[VarOr[Boolean]]): Int8 =
    if (xs.length != 8) throw new IllegalArgumentException("xs must have length 8")
    else Int8(Int4.from(xs.take(4)), Int4.from(xs.drop(4)))
}

implicit class VarOrInt8Ops(self: VarOr[Int8])(using u: Unify[Int8]) extends VarOrIntNOps[Int8](self, u) {
  override protected def consThis(x: VarOr[Int8]): VarOrInt8Ops = VarOrInt8Ops(x)

  def get: GoalWith[Int8] = for {
    (b0, b1, b2, b3) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b4, b5, b6, b7) <- fresh[Boolean, Boolean, Boolean, Boolean]
    result = Int8(Int4(b0, b1, b2, b3), Int4(b4, b5, b6, b7))
    _ <- self === result
  } yield result
}

final case class Int16(lo: Int8, hi: Int8) extends IntN[Int16] derives Unify {
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

implicit class VarOrInt16Ops(self: VarOr[Int16])(using u: Unify[Int16]) extends VarOrIntNOps[Int16](self, u) {
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

  def from(x: Int): Int16 = {
    if (x < 0 || x > 65535) throw new IllegalArgumentException("x must be in [0, 65535]")
    val lo = Int8.from((x & 0xff).toByte)
    val hi = Int8.from(((x >>> 8) & 0xff).toByte)
    Int16(lo, hi)
  }

  def from(xs: Vector[VarOr[Boolean]]): Int16 =
    if (xs.length != 16) throw new IllegalArgumentException("xs must have length 16")
    else Int16(Int8.from(xs.take(8)), Int8.from(xs.drop(8)))
}

final case class Int32(lo: Int16, hi: Int16) extends IntN[Int32] derives Unify {
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

implicit class VarOrInt32Ops(self: VarOr[Int32])(using u: Unify[Int32]) extends VarOrIntNOps[Int32](self, u) {
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

  def from(xs: Vector[VarOr[Boolean]]): Int32 =
    if (xs.length != 32) throw new IllegalArgumentException("xs must have length 32")
    else Int32(Int16.from(xs.take(16)), Int16.from(xs.drop(16)))
}

final case class Int64(lo: Int32, hi: Int32) extends IntN[Int64] derives Unify {
  override def plus(that: Int64): GoalWith[(VarOr[Boolean], Int64)] = for {
    (c, r) <- lo.plus(that.lo)
    (c2, r2) <- hi.plus(that.hi, c)
  } yield (c2, Int64(r, r2))

  override def plus(that: Int64, carry: VarOr[Boolean]): GoalWith[(VarOr[Boolean], Int64)] = for {
    (c, r) <- lo.plus(that.lo, carry)
    (c2, r2) <- hi.plus(that.hi, c)
  } yield (c2, Int64(r, r2))

  override inline def succ: GoalWith[(VarOr[Boolean], Int64)] = plus(Int64.one)

  override inline def unary_! : GoalWith[Int64] = for {
    l <- !lo
    h <- !hi
  } yield Int64(l, h)

  override inline def bits: Vector[VarOr[Boolean]] = lo.bits ++ hi.bits
}

implicit class VarOrInt64Ops(self: VarOr[Int64])(using u: Unify[Int64]) extends VarOrIntNOps[Int64](self, u) {
  override protected def consThis(x: VarOr[Int64]): VarOrIntNOps[Int64] = VarOrInt64Ops(x)

  override def get: GoalWith[Int64] = for {
    (b0, b1, b2, b3) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b4, b5, b6, b7) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b8, b9, b10, b11) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b12, b13, b14, b15) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b16, b17, b18, b19) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b20, b21, b22, b23) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b24, b25, b26, b27) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b28, b29, b30, b31) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b32, b33, b34, b35) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b36, b37, b38, b39) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b40, b41, b42, b43) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b44, b45, b46, b47) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b48, b49, b50, b51) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b52, b53, b54, b55) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b56, b57, b58, b59) <- fresh[Boolean, Boolean, Boolean, Boolean]
    (b60, b61, b62, b63) <- fresh[Boolean, Boolean, Boolean, Boolean]
    result = Int64(Int32(Int16(Int8(Int4(b0, b1, b2, b3), Int4(b4, b5, b6, b7)), Int8(Int4(b8, b9, b10, b11), Int4(b12, b13, b14, b15))), Int16(Int8(Int4(b16, b17, b18, b19), Int4(b20, b21, b22, b23)), Int8(Int4(b24, b25, b26, b27), Int4(b28, b29, b30, b31)))), Int32(Int16(Int8(Int4(b32, b33, b34, b35), Int4(b36, b37, b38, b39)), Int8(Int4(b40, b41, b42, b43), Int4(b44, b45, b46, b47))), Int16(Int8(Int4(b48, b49, b50, b51), Int4(b52, b53, b54, b55)), Int8(Int4(b56, b57, b58, b59), Int4(b60, b61, b62, b63)))))
    _ <- self === result
  } yield result
}

object Int64 {
  def zero = from(0)

  def one = from(1)

  def from(n: Long): Int64 = {
    val lo = Int32.from((n & 0xffffffff).toInt)
    val hi = Int32.from(((n >>> 32) & 0xffffffff).toInt)
    Int64(lo, hi)
  }

  def from(xs: Vector[VarOr[Boolean]]): Int64 =
    if (xs.length != 64) throw new IllegalArgumentException("xs must have length 64")
    else Int64(Int32.from(xs.take(32)), Int32.from(xs.drop(32)))
}


type BinaryNatVal = LList[Boolean]

final case class BinaryNat(xs: VarOr[LList[Boolean]]) extends Product1[VarOr[LList[Boolean]]] derives Unify {
  def succ: Rel[BinaryNatVal] = conde(
    xs.eqEmpty >> LList(true),
    for {
      (xh, xt) <- xs.is[Boolean, LList[Boolean]](LCons(_, _))
      (c, r) <- add(xh, true)
      rest <- c.elim {
        BinaryNat(xt).succ
      } {
        xt
      }
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

  def plus(that: VarOr[BinaryNatVal], c: VarOr[Boolean]): Rel[BinaryNatVal] = c.elim {
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

implicit class VarOrBinaryNatOps(self: VarOr[BinaryNat])(using u: Unify[BinaryNat]) {
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

final case class BinaryInt(sign: VarOr[Boolean], x: BinaryNat) extends Product2[VarOr[Boolean], BinaryNat] derives Unify

object BinaryInt {
  def from(n: BinaryNat): BinaryInt = BinaryInt(true, n)

  def from(n: Int): BinaryInt = if (n >= 0) from(BinaryNat.from(n)) else BinaryInt(false, BinaryNat.from((-n) - 1))
}

// nat with fixed size
sealed trait FixedNat {
  def list: VarOr[LList[Boolean]]
}

given Unify[FixedNat] with
  def concreteUnify(x: FixedNat, y: FixedNat): Unifying[Unit] = (x, y) match {
    case (FixedNatStatic(xs), FixedNatStatic(ys)) => xs.unify(ys)
    case _ => x.list.unify(y.list)
  }

final case class FixedNatStatic(xs: Vector[VarOr[Boolean]]) extends FixedNat {
  def list: VarOr[LList[Boolean]] = LList(xs *)
}

final case class FixedNatDynamic(list: VarOr[LList[Boolean]]) extends FixedNat {
  override def toString: String = list.getStrings match {
    case s: String => s"FixedNat($s)"
    case (s, xs) => bitsToNat(xs, s"FixedNat($s)")
  }

  def succ: Rel[FixedNat] = list.elim[FixedNat](FixedNat(LList())) { (x, xs) =>
    x.elim(for {
      tail <- FixedNat(xs).succ
      tail0 <- tail.is[LList[Boolean]](FixedNat(_))
    } yield FixedNat(false :: tail0))(Rel(FixedNat(true :: xs)))
  }

  def prev: Rel[FixedNat] = list.elim(Rel.failure[FixedNat]) { (x, xs) =>
    x.elim(Rel(FixedNat(false :: xs)))(for {
      tail <- FixedNat(xs).prev
      tail0 <- tail.is[LList[Boolean]](FixedNat(_))
    } yield FixedNat(true :: tail0))
  }

  def isZero: Goal = (list.elim(()) { (x, xs) =>
    for {
      _ <- x === false
      _ <- FixedNat(xs).isZero
    } yield ()
  }).goal
}

implicit class FixedNatOps(self: VarOr[FixedNat]) {
  def assumeLen(n: Int): Goal = self match {
    case FixedNatStatic(xs) => Goal.guard(xs.length == n)
    case _ => for {
      f <- FixedNat.create(n)
      _ <- f === self
    } yield ()
  }

  def append(other: VarOr[FixedNat]): Rel[FixedNat] = for {
    xs <- self.is[LList[Boolean]](FixedNat(_))
    ys <- other.is[LList[Boolean]](FixedNat(_))
    result <- xs.append(ys)
  } yield FixedNat(result)

  def succ: Rel[FixedNat] = for {
    xs <- self.is[LList[Boolean]](FixedNat(_))
    result <- FixedNatDynamic(xs).succ
  } yield result

  def prev: Rel[FixedNat] = for {
    xs <- self.is[LList[Boolean]](FixedNat(_))
    result <- FixedNatDynamic(xs).prev
  } yield result

  def isZero: Goal = for {
    xs <- self.is[LList[Boolean]](FixedNat(_))
    _ <- FixedNatDynamic(xs).isZero
  } yield ()

  def elim[U](whenZero: Rel[U])(whenSucc: VarOr[FixedNat] => Rel[U])(implicit u: Unify[U]): Rel[U] = conde(
    self.isZero >> whenZero,
    for {
      prev <- self.prev
      result <- whenSucc(prev)
    } yield result
  )

  // TODO: optimize me
  def +(other: VarOr[FixedNat]): Rel[FixedNat] = other.elim(self)(other0 => for {
    result0 <- self + other0
    result <- result0.succ
  } yield result)

  // TODO: optimize me
  def -(other: VarOr[FixedNat]): Rel[FixedNat] = other.elim(self)(other0 => for {
    result0 <- self - other0
    result <- result0.prev
  } yield result)

  def toLen(x: Int): GoalWith[Vector[VarOr[Boolean]]] = self match {
    case FixedNatStatic(xs) => Goal.guard(xs.length == x) >> GoalWith(xs)
    case _ => for {
      xs <- FixedNat.createAux(x)
      _ <- self === FixedNatStatic(xs)
    } yield xs
  }

  def to32: Rel[Int32] = for {
    xs <- toLen(32)
  } yield Int32.from(xs)
}

object FixedNat {
  def apply(list: VarOr[LList[Boolean]]): FixedNat = FixedNatDynamic(list)

  private[littlejian] def createAux(size: Int): GoalWith[Vector[VarOr[Boolean]]] =
    if (size < 0) throw new IllegalArgumentException("size must be non-negative")
    else if (size == 0) GoalWith(Vector())
    else for {
      head <- fresh[Boolean]
      tail <- createAux(size - 1)
    } yield head +: tail

  def create(size: Int): GoalWith[FixedNat] = for {
    xs <- createAux(size)
  } yield FixedNatStatic(xs)

  def from(bits: Int, value: Int): FixedNat = {
    val xs = (0 until bits).map(i => (value & (1 << i)) != 0).toVector
    FixedNatStatic(xs)
  }
}