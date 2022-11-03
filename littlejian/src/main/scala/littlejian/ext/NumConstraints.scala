package littlejian.ext

import littlejian.*

import scala.collection.immutable.NumericRange

implicit class VarOrByteOps(self: VarOr[Byte]) {
  def +(other: VarOr[Byte]): Rel[Byte] = for {
    result <- fresh[Byte]
    _ <- GoalNumOp2Byte(NumOp2.Add, self, other, result)
  } yield result

  def -(other: VarOr[Byte]): Rel[Byte] = for {
    result <- fresh[Byte]
    _ <- GoalNumOp2Byte(NumOp2.Sub, self, other, result)
  } yield result

  def *(other: VarOr[Byte]): Rel[Byte] = for {
    result <- fresh[Byte]
    _ <- GoalNumOp2Byte(NumOp2.Mul, self, other, result)
  } yield result

  def <(other: VarOr[Byte]): Goal = if (self.isInstanceOf[Var[_]]) GoalNumRangeByte(self, None, Some(Boundary(other, false))) else GoalNumRangeByte(other, None, Some(Boundary(self, false)))

  def <=(other: VarOr[Byte]): Goal = if (self.isInstanceOf[Var[_]]) GoalNumRangeByte(self, None, Some(Boundary(other, true))) else GoalNumRangeByte(other, None, Some(Boundary(self, true)))

  def >(other: VarOr[Byte]): Goal = if (self.isInstanceOf[Var[_]]) GoalNumRangeByte(self, Some(Boundary(other, false)), None) else GoalNumRangeByte(other, Some(Boundary(self, false)), None)

  def >=(other: VarOr[Byte]): Goal = if (self.isInstanceOf[Var[_]]) GoalNumRangeByte(self, Some(Boundary(other, true)), None) else GoalNumRangeByte(other, Some(Boundary(self, true)), None)

  def in(range: NumericRange[Byte]): GoalNumRange = if (range.step != 1) throw new IllegalArgumentException("step must be 1") else GoalNumRangeByte(self, Some(Boundary(range.start, true)), Some(Boundary(range.end, range.isInclusive)))
}

implicit class VarOrShortOps(self: VarOr[Short]) {
  def +(other: VarOr[Short]): Rel[Short] = for {
    result <- fresh[Short]
    _ <- GoalNumOp2Short(NumOp2.Add, self, other, result)
  } yield result

  def -(other: VarOr[Short]): Rel[Short] = for {
    result <- fresh[Short]
    _ <- GoalNumOp2Short(NumOp2.Sub, self, other, result)
  } yield result

  def *(other: VarOr[Short]): Rel[Short] = for {
    result <- fresh[Short]
    _ <- GoalNumOp2Short(NumOp2.Mul, self, other, result)
  } yield result

  def <(other: VarOr[Short]): Goal = if (self.isInstanceOf[Var[_]]) GoalNumRangeShort(self, None, Some(Boundary(other, false))) else GoalNumRangeShort(other, None, Some(Boundary(self, false)))

  def <=(other: VarOr[Short]): Goal = if (self.isInstanceOf[Var[_]]) GoalNumRangeShort(self, None, Some(Boundary(other, true))) else GoalNumRangeShort(other, None, Some(Boundary(self, true)))

  def >(other: VarOr[Short]): Goal = if (self.isInstanceOf[Var[_]]) GoalNumRangeShort(self, Some(Boundary(other, false)), None) else GoalNumRangeShort(other, Some(Boundary(self, false)), None)

  def >=(other: VarOr[Short]): Goal = if (self.isInstanceOf[Var[_]]) GoalNumRangeShort(self, Some(Boundary(other, true)), None) else GoalNumRangeShort(other, Some(Boundary(self, true)), None)

  def in(range: NumericRange[Short]): GoalNumRange = if (range.step != 1) throw new IllegalArgumentException("step must be 1") else GoalNumRangeShort(self, Some(Boundary(range.start, true)), Some(Boundary(range.end, range.isInclusive)))
}

implicit class VarOrIntOps(self: VarOr[Int]) {
  def +(other: VarOr[Int]): Rel[Int] = for {
    result <- fresh[Int]
    _ <- GoalNumOp2Int(NumOp2.Add, self, other, result)
  } yield result

  def -(other: VarOr[Int]): Rel[Int] = for {
    result <- fresh[Int]
    _ <- GoalNumOp2Int(NumOp2.Sub, self, other, result)
  } yield result

  def *(other: VarOr[Int]): Rel[Int] = for {
    result <- fresh[Int]
    _ <- GoalNumOp2Int(NumOp2.Mul, self, other, result)
  } yield result

  def <(other: VarOr[Int]): Goal = if (self.isInstanceOf[Var[_]]) GoalNumRangeInt(self, None, Some(Boundary(other, false))) else GoalNumRangeInt(other, None, Some(Boundary(self, false)))

  def <=(other: VarOr[Int]): Goal = if (self.isInstanceOf[Var[_]]) GoalNumRangeInt(self, None, Some(Boundary(other, true))) else GoalNumRangeInt(other, None, Some(Boundary(self, true)))

  def >(other: VarOr[Int]): Goal = if (self.isInstanceOf[Var[_]]) GoalNumRangeInt(self, Some(Boundary(other, false)), None) else GoalNumRangeInt(other, Some(Boundary(self, false)), None)

  def >=(other: VarOr[Int]): Goal = if (self.isInstanceOf[Var[_]]) GoalNumRangeInt(self, Some(Boundary(other, true)), None) else GoalNumRangeInt(other, Some(Boundary(self, true)), None)

  def in(range: NumericRange[Int]): GoalNumRange = if (range.step != 1) throw new IllegalArgumentException("step must be 1") else GoalNumRangeInt(self, Some(Boundary(range.start, true)), Some(Boundary(range.end, range.isInclusive)))
}

implicit class VarOrLongOps(self: VarOr[Long]) {
  def +(other: VarOr[Long]): Rel[Long] = for {
    result <- fresh[Long]
    _ <- GoalNumOp2Long(NumOp2.Add, self, other, result)
  } yield result

  def -(other: VarOr[Long]): Rel[Long] = for {
    result <- fresh[Long]
    _ <- GoalNumOp2Long(NumOp2.Sub, self, other, result)
  } yield result

  def *(other: VarOr[Long]): Rel[Long] = for {
    result <- fresh[Long]
    _ <- GoalNumOp2Long(NumOp2.Mul, self, other, result)
  } yield result

  def <(other: VarOr[Long]): Goal = if (self.isInstanceOf[Var[_]]) GoalNumRangeLong(self, None, Some(Boundary(other, false))) else GoalNumRangeLong(other, None, Some(Boundary(self, false)))

  def <=(other: VarOr[Long]): Goal = if (self.isInstanceOf[Var[_]]) GoalNumRangeLong(self, None, Some(Boundary(other, true))) else GoalNumRangeLong(other, None, Some(Boundary(self, true)))

  def >(other: VarOr[Long]): Goal = if (self.isInstanceOf[Var[_]]) GoalNumRangeLong(self, Some(Boundary(other, false)), None) else GoalNumRangeLong(other, Some(Boundary(self, false)), None)

  def >=(other: VarOr[Long]): Goal = if (self.isInstanceOf[Var[_]]) GoalNumRangeLong(self, Some(Boundary(other, true)), None) else GoalNumRangeLong(other, Some(Boundary(self, true)), None)

  def in(range: NumericRange[Long]): GoalNumRange = if (range.step != 1) throw new IllegalArgumentException("step must be 1") else GoalNumRangeLong(self, Some(Boundary(range.start, true)), Some(Boundary(range.end, range.isInclusive)))
}

implicit class VarOrFloatOps(self: VarOr[Float]) {
  def +(other: VarOr[Float]): Rel[Float] = for {
    result <- fresh[Float]
    _ <- GoalNumOp2Float(NumOp2.Add, self, other, result)
  } yield result

  def -(other: VarOr[Float]): Rel[Float] = for {
    result <- fresh[Float]
    _ <- GoalNumOp2Float(NumOp2.Sub, self, other, result)
  } yield result

  def *(other: VarOr[Float]): Rel[Float] = for {
    result <- fresh[Float]
    _ <- GoalNumOp2Float(NumOp2.Mul, self, other, result)
  } yield result

  def <(other: VarOr[Float]): Goal = if (self.isInstanceOf[Var[_]]) GoalNumRangeFloat(self, None, Some(Boundary(other, false))) else GoalNumRangeFloat(other, None, Some(Boundary(self, false)))

  def <=(other: VarOr[Float]): Goal = if (self.isInstanceOf[Var[_]]) GoalNumRangeFloat(self, None, Some(Boundary(other, true))) else GoalNumRangeFloat(other, None, Some(Boundary(self, true)))

  def >(other: VarOr[Float]): Goal = if (self.isInstanceOf[Var[_]]) GoalNumRangeFloat(self, Some(Boundary(other, false)), None) else GoalNumRangeFloat(other, Some(Boundary(self, false)), None)

  def >=(other: VarOr[Float]): Goal = if (self.isInstanceOf[Var[_]]) GoalNumRangeFloat(self, Some(Boundary(other, true)), None) else GoalNumRangeFloat(other, Some(Boundary(self, true)), None)
}

implicit class VarOrDoubleOps(self: VarOr[Double]) {
  def +(other: VarOr[Double]): Rel[Double] = for {
    result <- fresh[Double]
    _ <- GoalNumOp2Double(NumOp2.Add, self, other, result)
  } yield result

  def -(other: VarOr[Double]): Rel[Double] = for {
    result <- fresh[Double]
    _ <- GoalNumOp2Double(NumOp2.Sub, self, other, result)
  } yield result

  def *(other: VarOr[Double]): Rel[Double] = for {
    result <- fresh[Double]
    _ <- GoalNumOp2Double(NumOp2.Mul, self, other, result)
  } yield result

  def <(other: VarOr[Double]): Goal = if (self.isInstanceOf[Var[_]]) GoalNumRangeDouble(self, None, Some(Boundary(other, false))) else GoalNumRangeDouble(other, None, Some(Boundary(self, false)))

  def <=(other: VarOr[Double]): Goal = if (self.isInstanceOf[Var[_]]) GoalNumRangeDouble(self, None, Some(Boundary(other, true))) else GoalNumRangeDouble(other, None, Some(Boundary(self, true)))

  def >(other: VarOr[Double]): Goal = if (self.isInstanceOf[Var[_]]) GoalNumRangeDouble(self, Some(Boundary(other, false)), None) else GoalNumRangeDouble(other, Some(Boundary(self, false)), None)

  def >=(other: VarOr[Double]): Goal = if (self.isInstanceOf[Var[_]]) GoalNumRangeDouble(self, Some(Boundary(other, true)), None) else GoalNumRangeDouble(other, Some(Boundary(self, true)), None)
}