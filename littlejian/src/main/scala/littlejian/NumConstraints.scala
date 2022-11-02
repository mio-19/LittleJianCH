package littlejian

import scala.collection.immutable.NumericRange

type Num = Byte | Short | Int | Long | Float | Double

enum NumTag:
  case Byte
  case Short
  case Int
  case Long
  case Float
  case Double

implicit val U$Num: Unify[Num] = U$Union[Byte, Short, Int, Long, Float, Double]

enum NumOp2:
  case Add
  case Sub
  case Mul

sealed trait GoalNumOp2 extends GoalBasic {
  def rel: NumOp2

  def tag: NumTag

  def x: Num | Var[_ <: Num]

  def y: Num | Var[_ <: Num]

  def result: Num | Var[_ <: Num]

  override def execute(state: State): IterableOnce[State] = state.num.insert(state, this)

  def walk(subst: Subst): GoalNumOp2

  override def toString: String = {
    val relName = rel match {
      case NumOp2.Add => "+"
      case NumOp2.Sub => "-"
      case NumOp2.Mul => "*"
    }
    s"$x $relName $y === $result"
  }
}

object GoalNumOp2 {
  def unapply(self: GoalNumOp2): Some[(NumOp2, NumTag, Num | Var[_ <: Num], Num | Var[_ <: Num], Num | Var[_ <: Num])] = Some((self.rel, self.tag, self.x, self.y, self.result))
}

final case class GoalNumOp2Byte(rel: NumOp2, x: VarOr[Byte], y: VarOr[Byte], result: VarOr[Byte]) extends GoalNumOp2 {
  override def tag = NumTag.Byte

  override def walk(subst: Subst): GoalNumOp2Byte = GoalNumOp2Byte(rel, subst.walk(x), subst.walk(y), subst.walk(result))
}

final case class GoalNumOp2Short(rel: NumOp2, x: VarOr[Short], y: VarOr[Short], result: VarOr[Short]) extends GoalNumOp2 {
  override def tag = NumTag.Short

  override def walk(subst: Subst): GoalNumOp2Short = GoalNumOp2Short(rel, subst.walk(x), subst.walk(y), subst.walk(result))
}

final case class GoalNumOp2Int(rel: NumOp2, x: VarOr[Int], y: VarOr[Int], result: VarOr[Int]) extends GoalNumOp2 {
  override def tag = NumTag.Int

  override def walk(subst: Subst): GoalNumOp2Int = GoalNumOp2Int(rel, subst.walk(x), subst.walk(y), subst.walk(result))
}

final case class GoalNumOp2Long(rel: NumOp2, x: VarOr[Long], y: VarOr[Long], result: VarOr[Long]) extends GoalNumOp2 {
  override def tag = NumTag.Long

  override def walk(subst: Subst): GoalNumOp2Long = GoalNumOp2Long(rel, subst.walk(x), subst.walk(y), subst.walk(result))
}

final case class GoalNumOp2Float(rel: NumOp2, x: VarOr[Float], y: VarOr[Float], result: VarOr[Float]) extends GoalNumOp2 {
  override def tag = NumTag.Float

  override def walk(subst: Subst): GoalNumOp2Float = GoalNumOp2Float(rel, subst.walk(x), subst.walk(y), subst.walk(result))
}

final case class GoalNumOp2Double(rel: NumOp2, x: VarOr[Double], y: VarOr[Double], result: VarOr[Double]) extends GoalNumOp2 {
  override def tag = NumTag.Double

  override def walk(subst: Subst): GoalNumOp2Double = GoalNumOp2Double(rel, subst.walk(x), subst.walk(y), subst.walk(result))
}

final case class Boundary[T](x: T, eq: Boolean) {
  def lowMerge(other: Boundary[T])(implicit ordering: Ordering[T]) =
    if (ordering.gt(x, other.x)) this
    else if (ordering.equiv(x, other.x)) Boundary(x, eq && other.eq)
    else other

  def highMerge(other: Boundary[T])(implicit ordering: Ordering[T]) =
    if (ordering.lt(x, other.x)) this
    else if (ordering.equiv(x, other.x)) Boundary(x, eq && other.eq)
    else other
}

private def lowMerge[T](a: Boundary[VarOr[T]], b: Boundary[VarOr[T]])(implicit ordering: Ordering[T]): Boundary[VarOr[T]] =
  a.asInstanceOf[Boundary[T]].lowMerge(b.asInstanceOf[Boundary[T]]).asInstanceOf[Boundary[VarOr[T]]]

private def highMerge[T](a: Boundary[VarOr[T]], b: Boundary[VarOr[T]])(implicit ordering: Ordering[T]): Boundary[VarOr[T]] =
  a.asInstanceOf[Boundary[T]].highMerge(b.asInstanceOf[Boundary[T]]).asInstanceOf[Boundary[VarOr[T]]]

implicit class BoundaryVarOrOps[T](self: Boundary[VarOr[T]]) {
  def walk(subst: Subst): Boundary[VarOr[T]] = Boundary(subst.walk(self.x), self.eq)
}

sealed trait GoalNumRange extends GoalBasic {
  def tag: NumTag

  def low: Option[Boundary[_ <: Num | Var[_ <: Num]]]

  def high: Option[Boundary[_ <: Num | Var[_ <: Num]]]

  def num: Num | Var[_ <: Num]

  override def execute(state: State): IterableOnce[State] = state.num.insert(state, this)

  def walk(subst: Subst): GoalNumRange

  if (low.isEmpty && high.isEmpty) {
    throw new IllegalArgumentException("At least one boundary must be defined")
  }

  // with the same num
  def merge(other: GoalNumRange): Option[GoalNumRange]

  override def toString: String = (low, high) match {
    case (Some(Boundary(low, true)), None) => s"$low <= $num"
    case (Some(Boundary(low, false)), None) => s"$low < $num"
    case (Some(Boundary(low, true)), Some(Boundary(high, true))) => s"$low <= $num <= $high"
    case (Some(Boundary(low, false)), Some(Boundary(high, true))) => s"$low < $num <= $high"
    case (Some(Boundary(low, true)), Some(Boundary(high, false))) => s"$low <= $num < $high"
    case (Some(Boundary(low, false)), Some(Boundary(high, false))) => s"$low < $num < $high"
    case (None, Some(Boundary(high, true))) => s"$num <= $high"
    case (None, Some(Boundary(high, false))) => s"$num < $high"
    case _ => throw new IllegalStateException("impossible")
  }
}

private def merge(xs: Seq[GoalNumRange]): Vector[GoalNumRange] = {
  val tail = xs.tail
  if (tail.isEmpty) Vector(xs.head)
  else doMerge(merge(tail), xs.head)
}
private def doMerge(xs: Vector[GoalNumRange], x: GoalNumRange): Vector[GoalNumRange] = {
  val head = xs.head
  val tail = xs.tail
  if (tail.isEmpty) {
    head.merge(x) match {
      case Some(merged) => Vector(merged)
      case None => Vector(head, x)
    }
  } else {
    head.merge(x) match {
      case Some(merged) => doMerge(tail, merged)
      case None => head +: doMerge(tail, x)
    }
  }
}

final case class GoalNumRangeByte(num: VarOr[Byte], low: Option[Boundary[VarOr[Byte]]], high: Option[Boundary[VarOr[Byte]]]) extends GoalNumRange {
  override def tag = NumTag.Byte

  override def walk(subst: Subst): GoalNumRangeByte = GoalNumRangeByte(num = subst.walk(num), low = low.map(_.walk(subst)), high = high.map(_.walk(subst)))

  override def merge(other: GoalNumRange): Option[GoalNumRange] = {
    if (num != other.num) throw new IllegalArgumentException("num must be the same")
    val o = other.asInstanceOf[GoalNumRangeByte]
    (this, o) match {
      case (GoalNumRangeByte(_, None, high), GoalNumRangeByte(_, low, None)) => Some(GoalNumRangeByte(num, low, high))
      case (GoalNumRangeByte(_, low, None), GoalNumRangeByte(_, None, high)) => Some(GoalNumRangeByte(num, low, high))
      case (GoalNumRangeByte(_, Some(low1@Boundary(_: Byte, _)), Some(high1@Boundary(_: Byte, _))), GoalNumRangeByte(_, Some(low2@Boundary(_: Byte, _)), Some(high2@Boundary(_: Byte, _)))) =>
        Some(GoalNumRangeByte(num, Some(lowMerge(low1, low2)), Some(highMerge(high1, high2))))
      case (GoalNumRangeByte(_, Some(low1@Boundary(_: Byte, _)), Some(high1@Boundary(_: Byte, _))), GoalNumRangeByte(_, Some(low2@Boundary(_: Byte, _)), None)) =>
        Some(GoalNumRangeByte(num, Some(lowMerge(low1, low2)), Some(high1)))
      case (GoalNumRangeByte(_, Some(low1@Boundary(_: Byte, _)), None), GoalNumRangeByte(_, Some(low2@Boundary(_: Byte, _)), Some(high2@Boundary(_: Byte, _)))) =>
        Some(GoalNumRangeByte(num, Some(lowMerge(low1, low2)), Some(high2)))
      case (GoalNumRangeByte(_, None, Some(high1@Boundary(_: Byte, _))), GoalNumRangeByte(_, Some(low2@Boundary(_: Byte, _)), Some(high2@Boundary(_: Byte, _)))) =>
        Some(GoalNumRangeByte(num, Some(low2), Some(highMerge(high1, high2))))
      case (GoalNumRangeByte(_, Some(low1@Boundary(_: Byte, _)), Some(high1@Boundary(_: Byte, _))), GoalNumRangeByte(_, None, Some(high2@Boundary(_: Byte, _)))) =>
        Some(GoalNumRangeByte(num, Some(low1), Some(highMerge(high1, high2))))
      case _ => None
    }
  }
}

final case class GoalNumRangeShort(num: VarOr[Short], low: Option[Boundary[VarOr[Short]]], high: Option[Boundary[VarOr[Short]]]) extends GoalNumRange {
  override def tag = NumTag.Short

  override def walk(subst: Subst): GoalNumRangeShort = GoalNumRangeShort(num = subst.walk(num), low = low.map(_.walk(subst)), high = high.map(_.walk(subst)))

  override def merge(other: GoalNumRange): Option[GoalNumRange] = {
    if (num != other.num) throw new IllegalArgumentException("num must be the same")
    val o = other.asInstanceOf[GoalNumRangeShort]
    (this, o) match {
      case (GoalNumRangeShort(_, None, high), GoalNumRangeShort(_, low, None)) => Some(GoalNumRangeShort(num, low, high))
      case (GoalNumRangeShort(_, low, None), GoalNumRangeShort(_, None, high)) => Some(GoalNumRangeShort(num, low, high))
      case (GoalNumRangeShort(_, Some(low1@Boundary(_: Short, _)), Some(high1@Boundary(_: Short, _))), GoalNumRangeShort(_, Some(low2@Boundary(_: Short, _)), Some(high2@Boundary(_: Short, _)))) =>
        Some(GoalNumRangeShort(num, Some(lowMerge(low1, low2)), Some(highMerge(high1, high2))))
      case (GoalNumRangeShort(_, Some(low1@Boundary(_: Short, _)), Some(high1@Boundary(_: Short, _))), GoalNumRangeShort(_, Some(low2@Boundary(_: Short, _)), None)) =>
        Some(GoalNumRangeShort(num, Some(lowMerge(low1, low2)), Some(high1)))
      case (GoalNumRangeShort(_, Some(low1@Boundary(_: Short, _)), None), GoalNumRangeShort(_, Some(low2@Boundary(_: Short, _)), Some(high2@Boundary(_: Short, _)))) =>
        Some(GoalNumRangeShort(num, Some(lowMerge(low1, low2)), Some(high2)))
      case (GoalNumRangeShort(_, None, Some(high1@Boundary(_: Short, _))), GoalNumRangeShort(_, Some(low2@Boundary(_: Short, _)), Some(high2@Boundary(_: Short, _)))) =>
        Some(GoalNumRangeShort(num, Some(low2), Some(highMerge(high1, high2))))
      case (GoalNumRangeShort(_, Some(low1@Boundary(_: Short, _)), Some(high1@Boundary(_: Short, _))), GoalNumRangeShort(_, None, Some(high2@Boundary(_: Short, _)))) =>
        Some(GoalNumRangeShort(num, Some(low1), Some(highMerge(high1, high2))))
      case _ => None
    }
  }
}

final case class GoalNumRangeInt(num: VarOr[Int], low: Option[Boundary[VarOr[Int]]], high: Option[Boundary[VarOr[Int]]]) extends GoalNumRange {
  override def tag = NumTag.Int

  override def walk(subst: Subst): GoalNumRangeInt = GoalNumRangeInt(num = subst.walk(num), low = low.map(_.walk(subst)), high = high.map(_.walk(subst)))

  override def merge(other: GoalNumRange): Option[GoalNumRange] = {
    if (num != other.num) throw new IllegalArgumentException("num must be the same")
    val o = other.asInstanceOf[GoalNumRangeInt]
    (this, o) match {
      case (GoalNumRangeInt(_, None, high), GoalNumRangeInt(_, low, None)) => Some(GoalNumRangeInt(num, low, high))
      case (GoalNumRangeInt(_, low, None), GoalNumRangeInt(_, None, high)) => Some(GoalNumRangeInt(num, low, high))
      case (GoalNumRangeInt(_, Some(low1@Boundary(_: Int, _)), Some(high1@Boundary(_: Int, _))), GoalNumRangeInt(_, Some(low2@Boundary(_: Int, _)), Some(high2@Boundary(_: Int, _)))) =>
        Some(GoalNumRangeInt(num, Some(lowMerge(low1, low2)), Some(highMerge(high1, high2))))
      case (GoalNumRangeInt(_, Some(low1@Boundary(_: Int, _)), Some(high1@Boundary(_: Int, _))), GoalNumRangeInt(_, Some(low2@Boundary(_: Int, _)), None)) =>
        Some(GoalNumRangeInt(num, Some(lowMerge(low1, low2)), Some(high1)))
      case (GoalNumRangeInt(_, Some(low1@Boundary(_: Int, _)), None), GoalNumRangeInt(_, Some(low2@Boundary(_: Int, _)), Some(high2@Boundary(_: Int, _)))) =>
        Some(GoalNumRangeInt(num, Some(lowMerge(low1, low2)), Some(high2)))
      case (GoalNumRangeInt(_, None, Some(high1@Boundary(_: Int, _))), GoalNumRangeInt(_, Some(low2@Boundary(_: Int, _)), Some(high2@Boundary(_: Int, _)))) =>
        Some(GoalNumRangeInt(num, Some(low2), Some(highMerge(high1, high2))))
      case (GoalNumRangeInt(_, Some(low1@Boundary(_: Int, _)), Some(high1@Boundary(_: Int, _))), GoalNumRangeInt(_, None, Some(high2@Boundary(_: Int, _)))) =>
        Some(GoalNumRangeInt(num, Some(low1), Some(highMerge(high1, high2))))
      case _ => None
    }
  }
}

final case class GoalNumRangeLong(num: VarOr[Long], low: Option[Boundary[VarOr[Long]]], high: Option[Boundary[VarOr[Long]]]) extends GoalNumRange {
  override def tag = NumTag.Long

  override def walk(subst: Subst): GoalNumRangeLong = GoalNumRangeLong(num = subst.walk(num), low = low.map(_.walk(subst)), high = high.map(_.walk(subst)))

  override def merge(other: GoalNumRange): Option[GoalNumRange] = {
    if (num != other.num) throw new IllegalArgumentException("num must be the same")
    val o = other.asInstanceOf[GoalNumRangeLong]
    (this, o) match {
      case (GoalNumRangeLong(_, None, high), GoalNumRangeLong(_, low, None)) => Some(GoalNumRangeLong(num, low, high))
      case (GoalNumRangeLong(_, low, None), GoalNumRangeLong(_, None, high)) => Some(GoalNumRangeLong(num, low, high))
      case (GoalNumRangeLong(_, Some(low1@Boundary(_: Long, _)), Some(high1@Boundary(_: Long, _))), GoalNumRangeLong(_, Some(low2@Boundary(_: Long, _)), Some(high2@Boundary(_: Long, _)))) =>
        Some(GoalNumRangeLong(num, Some(lowMerge(low1, low2)), Some(highMerge(high1, high2))))
      case (GoalNumRangeLong(_, Some(low1@Boundary(_: Long, _)), Some(high1@Boundary(_: Long, _))), GoalNumRangeLong(_, Some(low2@Boundary(_: Long, _)), None)) =>
        Some(GoalNumRangeLong(num, Some(lowMerge(low1, low2)), Some(high1)))
      case (GoalNumRangeLong(_, Some(low1@Boundary(_: Long, _)), None), GoalNumRangeLong(_, Some(low2@Boundary(_: Long, _)), Some(high2@Boundary(_: Long, _)))) =>
        Some(GoalNumRangeLong(num, Some(lowMerge(low1, low2)), Some(high2)))
      case (GoalNumRangeLong(_, None, Some(high1@Boundary(_: Long, _))), GoalNumRangeLong(_, Some(low2@Boundary(_: Long, _)), Some(high2@Boundary(_: Long, _)))) =>
        Some(GoalNumRangeLong(num, Some(low2), Some(highMerge(high1, high2))))
      case (GoalNumRangeLong(_, Some(low1@Boundary(_: Long, _)), Some(high1@Boundary(_: Long, _))), GoalNumRangeLong(_, None, Some(high2@Boundary(_: Long, _)))) =>
        Some(GoalNumRangeLong(num, Some(low1), Some(highMerge(high1, high2))))
      case _ => None
    }
  }
}

final case class GoalNumRangeFloat(num: VarOr[Float], low: Option[Boundary[VarOr[Float]]], high: Option[Boundary[VarOr[Float]]]) extends GoalNumRange {
  override def tag = NumTag.Float

  override def walk(subst: Subst): GoalNumRangeFloat = GoalNumRangeFloat(num = subst.walk(num), low = low.map(_.walk(subst)), high = high.map(_.walk(subst)))

  override def merge(other: GoalNumRange): Option[GoalNumRange] = {
    if (num != other.num) throw new IllegalArgumentException("num must be the same")
    val o = other.asInstanceOf[GoalNumRangeFloat]
    (this, o) match {
      case (GoalNumRangeFloat(_, None, high), GoalNumRangeFloat(_, low, None)) => Some(GoalNumRangeFloat(num, low, high))
      case (GoalNumRangeFloat(_, low, None), GoalNumRangeFloat(_, None, high)) => Some(GoalNumRangeFloat(num, low, high))
      case (GoalNumRangeFloat(_, Some(low1@Boundary(_: Float, _)), Some(high1@Boundary(_: Float, _))), GoalNumRangeFloat(_, Some(low2@Boundary(_: Float, _)), Some(high2@Boundary(_: Float, _)))) =>
        Some(GoalNumRangeFloat(num, Some(lowMerge(low1, low2)), Some(highMerge(high1, high2))))
      case (GoalNumRangeFloat(_, Some(low1@Boundary(_: Float, _)), Some(high1@Boundary(_: Float, _))), GoalNumRangeFloat(_, Some(low2@Boundary(_: Float, _)), None)) =>
        Some(GoalNumRangeFloat(num, Some(lowMerge(low1, low2)), Some(high1)))
      case (GoalNumRangeFloat(_, Some(low1@Boundary(_: Float, _)), None), GoalNumRangeFloat(_, Some(low2@Boundary(_: Float, _)), Some(high2@Boundary(_: Float, _)))) =>
        Some(GoalNumRangeFloat(num, Some(lowMerge(low1, low2)), Some(high2)))
      case (GoalNumRangeFloat(_, None, Some(high1@Boundary(_: Float, _))), GoalNumRangeFloat(_, Some(low2@Boundary(_: Float, _)), Some(high2@Boundary(_: Float, _)))) =>
        Some(GoalNumRangeFloat(num, Some(low2), Some(highMerge(high1, high2))))
      case (GoalNumRangeFloat(_, Some(low1@Boundary(_: Float, _)), Some(high1@Boundary(_: Float, _))), GoalNumRangeFloat(_, None, Some(high2@Boundary(_: Float, _)))) =>
        Some(GoalNumRangeFloat(num, Some(low1), Some(highMerge(high1, high2))))
      case _ => None
    }
  }
}

final case class GoalNumRangeDouble(num: VarOr[Double], low: Option[Boundary[VarOr[Double]]], high: Option[Boundary[VarOr[Double]]]) extends GoalNumRange {
  override def tag = NumTag.Double

  override def walk(subst: Subst): GoalNumRangeDouble = GoalNumRangeDouble(num = subst.walk(num), low = low.map(_.walk(subst)), high = high.map(_.walk(subst)))

  override def merge(other: GoalNumRange): Option[GoalNumRange] = {
    if (num != other.num) throw new IllegalArgumentException("num must be the same")
    val o = other.asInstanceOf[GoalNumRangeDouble]
    (this, o) match {
      case (GoalNumRangeDouble(_, None, high), GoalNumRangeDouble(_, low, None)) => Some(GoalNumRangeDouble(num, low, high))
      case (GoalNumRangeDouble(_, low, None), GoalNumRangeDouble(_, None, high)) => Some(GoalNumRangeDouble(num, low, high))
      case (GoalNumRangeDouble(_, Some(low1@Boundary(_: Double, _)), Some(high1@Boundary(_: Double, _))), GoalNumRangeDouble(_, Some(low2@Boundary(_: Double, _)), Some(high2@Boundary(_: Double, _)))) =>
        Some(GoalNumRangeDouble(num, Some(lowMerge(low1, low2)), Some(highMerge(high1, high2))))
      case (GoalNumRangeDouble(_, Some(low1@Boundary(_: Double, _)), Some(high1@Boundary(_: Double, _))), GoalNumRangeDouble(_, Some(low2@Boundary(_: Double, _)), None)) =>
        Some(GoalNumRangeDouble(num, Some(lowMerge(low1, low2)), Some(high1)))
      case (GoalNumRangeDouble(_, Some(low1@Boundary(_: Double, _)), None), GoalNumRangeDouble(_, Some(low2@Boundary(_: Double, _)), Some(high2@Boundary(_: Double, _)))) =>
        Some(GoalNumRangeDouble(num, Some(lowMerge(low1, low2)), Some(high2)))
      case (GoalNumRangeDouble(_, None, Some(high1@Boundary(_: Double, _))), GoalNumRangeDouble(_, Some(low2@Boundary(_: Double, _)), Some(high2@Boundary(_: Double, _)))) =>
        Some(GoalNumRangeDouble(num, Some(low2), Some(highMerge(high1, high2))))
      case (GoalNumRangeDouble(_, Some(low1@Boundary(_: Double, _)), Some(high1@Boundary(_: Double, _))), GoalNumRangeDouble(_, None, Some(high2@Boundary(_: Double, _)))) =>
        Some(GoalNumRangeDouble(num, Some(low1), Some(highMerge(high1, high2))))
      case _ => None
    }
  }
}

implicit class GoalNumOpOps(self: GoalNumOp2) {
  def is2: Boolean = {
    val a = if (self.x.isInstanceOf[Num]) 1 else 0
    val b = if (self.y.isInstanceOf[Num]) 1 else 0
    val c = if (self.result.isInstanceOf[Num]) 1 else 0
    a + b + c >= 2
  }

  def solve2: Unifying[Unit] = self match {
    case GoalNumOp2Byte(NumOp2.Add, x: Byte, y: Byte, rel) => rel.unify((x + y).asInstanceOf[Byte])
    case GoalNumOp2Byte(NumOp2.Add, x, y: Byte, rel: Byte) => x.unify((rel - y).asInstanceOf[Byte])
    case GoalNumOp2Byte(NumOp2.Add, x: Byte, y, rel: Byte) => y.unify((rel - x).asInstanceOf[Byte])
    case GoalNumOp2Byte(NumOp2.Sub, x: Byte, y: Byte, rel) => rel.unify((x - y).asInstanceOf[Byte])
    case GoalNumOp2Byte(NumOp2.Sub, x, y: Byte, rel: Byte) => x.unify((rel + y).asInstanceOf[Byte])
    case GoalNumOp2Byte(NumOp2.Sub, x: Byte, y, rel: Byte) => y.unify((x - rel).asInstanceOf[Byte])
    case GoalNumOp2Byte(NumOp2.Mul, x: Byte, y: Byte, rel) => rel.unify((x * y).asInstanceOf[Byte])
    case GoalNumOp2Byte(NumOp2.Mul, x, y: Byte, rel: Byte) => if (rel % y == 0) x.unify((rel / y).asInstanceOf[Byte]) else Unifying.failure
    case GoalNumOp2Byte(NumOp2.Mul, x: Byte, y, rel: Byte) => if (rel % x == 0) y.unify((rel / x).asInstanceOf[Byte]) else Unifying.failure
    case GoalNumOp2Byte(_, _, _, _) => throw new IllegalArgumentException("not a 2-arg goal")
    case GoalNumOp2Short(NumOp2.Add, x: Short, y: Short, rel) => rel.unify((x + y).asInstanceOf[Short])
    case GoalNumOp2Short(NumOp2.Add, x, y: Short, rel: Short) => x.unify((rel - y).asInstanceOf[Short])
    case GoalNumOp2Short(NumOp2.Add, x: Short, y, rel: Short) => y.unify((rel - x).asInstanceOf[Short])
    case GoalNumOp2Short(NumOp2.Sub, x: Short, y: Short, rel) => rel.unify((x - y).asInstanceOf[Short])
    case GoalNumOp2Short(NumOp2.Sub, x, y: Short, rel: Short) => x.unify((rel + y).asInstanceOf[Short])
    case GoalNumOp2Short(NumOp2.Sub, x: Short, y, rel: Short) => y.unify((x - rel).asInstanceOf[Short])
    case GoalNumOp2Short(NumOp2.Mul, x: Short, y: Short, rel) => rel.unify((x * y).asInstanceOf[Short])
    case GoalNumOp2Short(NumOp2.Mul, x, y: Short, rel: Short) => if (rel % y == 0) x.unify((rel / y).asInstanceOf[Short]) else Unifying.failure
    case GoalNumOp2Short(NumOp2.Mul, x: Short, y, rel: Short) => if (rel % x == 0) y.unify((rel / x).asInstanceOf[Short]) else Unifying.failure
    case GoalNumOp2Short(_, _, _, _) => throw new IllegalArgumentException("not a 2-arg goal")
    case GoalNumOp2Int(NumOp2.Add, x: Int, y: Int, rel) => rel.unify(x + y)
    case GoalNumOp2Int(NumOp2.Add, x, y: Int, rel: Int) => x.unify(rel - y)
    case GoalNumOp2Int(NumOp2.Add, x: Int, y, rel: Int) => y.unify(rel - x)
    case GoalNumOp2Int(NumOp2.Sub, x: Int, y: Int, rel) => rel.unify(x - y)
    case GoalNumOp2Int(NumOp2.Sub, x, y: Int, rel: Int) => x.unify(rel + y)
    case GoalNumOp2Int(NumOp2.Sub, x: Int, y, rel: Int) => y.unify(x - rel)
    case GoalNumOp2Int(NumOp2.Mul, x: Int, y: Int, rel) => rel.unify(x * y)
    case GoalNumOp2Int(NumOp2.Mul, x, y: Int, rel: Int) => if (rel % y == 0) x.unify(rel / y) else Unifying.failure
    case GoalNumOp2Int(NumOp2.Mul, x: Int, y, rel: Int) => if (rel % x == 0) y.unify(rel / x) else Unifying.failure
    case GoalNumOp2Int(_, _, _, _) => throw new IllegalArgumentException("not a 2-arg goal")
    case GoalNumOp2Long(NumOp2.Add, x: Long, y: Long, rel) => rel.unify(x + y)
    case GoalNumOp2Long(NumOp2.Add, x, y: Long, rel: Long) => x.unify(rel - y)
    case GoalNumOp2Long(NumOp2.Add, x: Long, y, rel: Long) => y.unify(rel - x)
    case GoalNumOp2Long(NumOp2.Sub, x: Long, y: Long, rel) => rel.unify(x - y)
    case GoalNumOp2Long(NumOp2.Sub, x, y: Long, rel: Long) => x.unify(rel + y)
    case GoalNumOp2Long(NumOp2.Sub, x: Long, y, rel: Long) => y.unify(x - rel)
    case GoalNumOp2Long(NumOp2.Mul, x: Long, y: Long, rel) => rel.unify(x * y)
    case GoalNumOp2Long(NumOp2.Mul, x, y: Long, rel: Long) => if (rel % y == 0) x.unify(rel / y) else Unifying.failure
    case GoalNumOp2Long(NumOp2.Mul, x: Long, y, rel: Long) => if (rel % x == 0) y.unify(rel / x) else Unifying.failure
    case GoalNumOp2Long(_, _, _, _) => throw new IllegalArgumentException("not a 2-arg goal")
    case GoalNumOp2Float(NumOp2.Add, x: Float, y: Float, rel) => rel.unify(x + y)
    case GoalNumOp2Float(NumOp2.Add, x, y: Float, rel: Float) => x.unify(rel - y)
    case GoalNumOp2Float(NumOp2.Add, x: Float, y, rel: Float) => y.unify(rel - x)
    case GoalNumOp2Float(NumOp2.Sub, x: Float, y: Float, rel) => rel.unify(x - y)
    case GoalNumOp2Float(NumOp2.Sub, x, y: Float, rel: Float) => x.unify(rel + y)
    case GoalNumOp2Float(NumOp2.Sub, x: Float, y, rel: Float) => y.unify(x - rel)
    case GoalNumOp2Float(NumOp2.Mul, x: Float, y: Float, rel) => rel.unify(x * y)
    case GoalNumOp2Float(NumOp2.Mul, x, y: Float, rel: Float) => x.unify(rel / y)
    case GoalNumOp2Float(NumOp2.Mul, x: Float, y, rel: Float) => y.unify(rel / x)
    case GoalNumOp2Float(_, _, _, _) => throw new IllegalArgumentException("not a 2-arg goal")
    case GoalNumOp2Double(NumOp2.Add, x: Double, y: Double, rel) => rel.unify(x + y)
    case GoalNumOp2Double(NumOp2.Add, x, y: Double, rel: Double) => x.unify(rel - y)
    case GoalNumOp2Double(NumOp2.Add, x: Double, y, rel: Double) => y.unify(rel - x)
    case GoalNumOp2Double(NumOp2.Sub, x: Double, y: Double, rel) => rel.unify(x - y)
    case GoalNumOp2Double(NumOp2.Sub, x, y: Double, rel: Double) => x.unify(rel + y)
    case GoalNumOp2Double(NumOp2.Sub, x: Double, y, rel: Double) => y.unify(x - rel)
    case GoalNumOp2Double(NumOp2.Mul, x: Double, y: Double, rel) => rel.unify(x * y)
    case GoalNumOp2Double(NumOp2.Mul, x, y: Double, rel: Double) => x.unify(rel / y)
    case GoalNumOp2Double(NumOp2.Mul, x: Double, y, rel: Double) => y.unify(rel / x)
    case GoalNumOp2Double(_, _, _, _) => throw new IllegalArgumentException("not a 2-arg goal")
  }
}

implicit class GoalNumRangeOps(self: GoalNumRange) {

  import math.Ordered._

  private def check[T <: Num](num: T, low: Option[Boundary[VarOr[T]]], high: Option[Boundary[VarOr[T]]])(implicit order: Ordering[T]): Boolean =
    check0(num, low.asInstanceOf[Option[Boundary[T]]], high.asInstanceOf[Option[Boundary[T]]])

  private def check0[T <: Num](num: T, low: Option[Boundary[T]], high: Option[Boundary[T]])(implicit order: Ordering[T]): Boolean = {
    val lowOk = low match {
      case None => true
      case Some(Boundary(b, true)) => num >= b
      case Some(Boundary(b, false)) => num > b
    }
    val highOk = high match {
      case None => true
      case Some(Boundary(b, true)) => num <= b
      case Some(Boundary(b, false)) => num < b
    }
    lowOk && highOk
  }

  private def guard(x: Boolean): Unifying[Option[GoalNumRange]] = Unifying.guard(x) >> Unifying.success(None)

  private def doInline0[T <: Num](x: Var[T], low: Boundary[T], high: Boundary[T])(implicit unify: Unify[T], num: Integral[T]): Vector[Unifying[Unit]] = {
    var lowX = low.x
    if (!low.eq) lowX = num.plus(lowX, num.one)
    var highX = high.x
    if (!high.eq) highX = num.minus(highX, num.one)
    NumericRange.inclusive(lowX, highX, num.one).toVector.map(_.unify(x))
  }

  private def doInline[T <: Num](x: Var[T], low: Boundary[VarOr[T]], high: Boundary[VarOr[T]])(implicit unify: Unify[T], num: Integral[T]): Vector[Unifying[Unit]] =
    doInline0(x, low.asInstanceOf[Boundary[T]], high.asInstanceOf[Boundary[T]])

  inline val inlineLimit = 10

  def reduce: Vector[Unifying[Unit]] | Unifying[Option[GoalNumRange]] = self match {
    case GoalNumRangeByte(num: Byte, Some(low@Boundary(_: Byte, _)), Some(high@Boundary(_: Byte, _))) => guard(check(num, Some(low), Some(high)))
    case GoalNumRangeByte(num: Byte, None, Some(high@Boundary(_: Byte, _))) => guard(check(num, None, Some(high)))
    case GoalNumRangeByte(num: Byte, Some(low@Boundary(_: Byte, _)), None) => guard(check(num, Some(low), None))
    case GoalNumRangeShort(num: Short, Some(low@Boundary(_: Short, _)), Some(high@Boundary(_: Short, _))) => guard(check(num, Some(low), Some(high)))
    case GoalNumRangeShort(num: Short, None, Some(high@Boundary(_: Short, _))) => guard(check(num, None, Some(high)))
    case GoalNumRangeShort(num: Short, Some(low@Boundary(_: Short, _)), None) => guard(check(num, Some(low), None))
    case GoalNumRangeInt(num: Int, Some(low@Boundary(_: Int, _)), Some(high@Boundary(_: Int, _))) => guard(check(num, Some(low), Some(high)))
    case GoalNumRangeInt(num: Int, None, Some(high@Boundary(_: Int, _))) => guard(check(num, None, Some(high)))
    case GoalNumRangeInt(num: Int, Some(low@Boundary(_: Int, _)), None) => guard(check(num, Some(low), None))
    case GoalNumRangeLong(num: Long, Some(low@Boundary(_: Long, _)), Some(high@Boundary(_: Long, _))) => guard(check(num, Some(low), Some(high)))
    case GoalNumRangeLong(num: Long, None, Some(high@Boundary(_: Long, _))) => guard(check(num, None, Some(high)))
    case GoalNumRangeLong(num: Long, Some(low@Boundary(_: Long, _)), None) => guard(check(num, Some(low), None))
    case GoalNumRangeFloat(num: Float, Some(low@Boundary(_: Float, _)), Some(high@Boundary(_: Float, _))) => guard(check(num, Some(low), Some(high)))
    case GoalNumRangeFloat(num: Float, None, Some(high@Boundary(_: Float, _))) => guard(check(num, None, Some(high)))
    case GoalNumRangeFloat(num: Float, Some(low@Boundary(_: Float, _)), None) => guard(check(num, Some(low), None))
    case GoalNumRangeDouble(num: Double, Some(low@Boundary(_: Double, _)), Some(high@Boundary(_: Double, _))) => guard(check(num, Some(low), Some(high)))
    case GoalNumRangeDouble(num: Double, None, Some(high@Boundary(_: Double, _))) => guard(check(num, None, Some(high)))
    case GoalNumRangeDouble(num: Double, Some(low@Boundary(_: Double, _)), None) => guard(check(num, Some(low), None))
    // expand small ranges
    case GoalNumRangeByte(num: Var[Byte], Some(low@Boundary(l: Byte, _)), Some(high@Boundary(h: Byte, _))) if (h - l < inlineLimit) => doInline(num, low, high)
    case GoalNumRangeShort(num: Var[Short], Some(low@Boundary(l: Short, _)), Some(high@Boundary(h: Short, _))) if (h - l < inlineLimit) => doInline(num, low, high)
    case GoalNumRangeInt(num: Var[Int], Some(low@Boundary(l: Int, _)), Some(high@Boundary(h: Int, _))) if (h - l < inlineLimit) => doInline(num, low, high)
    case GoalNumRangeLong(num: Var[Long], Some(low@Boundary(l: Long, _)), Some(high@Boundary(h: Long, _))) if (h - l < inlineLimit) => doInline(num, low, high)
    case x => Unifying.success(Some(x))
  }

  def reduce(subst: Subst): Option[(Subst, Option[GoalNumRange])] = this.reduce match {
    case v: Vector[_] => Unifying.runAll(v).getSubst(subst).map((_, None)) // TODO: fix me
    case unify: Unifying[_] => unify.run(subst) match {
      case Some((subst, _), v) => Some(subst, v)
      case None => None
    }
  }
}

final case class NumState(op2s: Vector[GoalNumOp2], ranges: Vector[GoalNumRange]) {
  def insert(state: State, x: GoalNumOp2): IterableOnce[State] = copy(op2s = x +: op2s).onInsert(state)

  def insert(state: State, x: GoalNumRange): IterableOnce[State] = copy(ranges = x +: ranges).onInsert(state)

  def onEq(eq: EqState): IterableOnce[(EqState, NumState)] = for {
    (subst, ranges) <- NumState.runRanges(eq.subst, NumState.rangesMerge(ranges))
    (subst, op2s) <- NumState.runOp2s(subst, op2s)
  } yield (EqState(subst), NumState(op2s = op2s, ranges = ranges))

  def onInsert(state: State): IterableOnce[State] = this.onEq(state.eq) map {
    case (eq, num) => state.copy(eq = eq, num = num)
  }

  def print: String = op2s.map(_.toString).appendedAll(ranges.map(_.toString)).mkString(" && ")
}

object NumState {
  def runOp2s(subst: Subst, op2s: Vector[GoalNumOp2]): Option[(Subst, Vector[GoalNumOp2])] = if (op2s.isEmpty) Some(subst, op2s) else // optimize
  {
    val (cl2, rest) = op2s.map(_.walk(subst)).partition(_.is2)
    Unifying.runAll(cl2.map(_.solve2)).getSubst(subst) map { subst =>
      (subst, rest)
    }
  }

  def runRanges(subst: Subst, ranges: Vector[GoalNumRange]): Option[(Subst, Vector[GoalNumRange])] =
    if (ranges.isEmpty)
      Some(subst, ranges)
    else
      for {
        (subst, maybeRange) <- ranges.head.walk(subst).reduce(subst)
        (subst, rest) <- runRanges(subst, ranges.tail)
      } yield (subst, maybeRange ++: rest)

  def rangesMerge(ranges: Vector[GoalNumRange]): Vector[GoalNumRange] = ranges.groupBy(_.num).values.toVector.flatMap(merge)

  val empty: NumState = NumState(Vector.empty, Vector.empty)
}
