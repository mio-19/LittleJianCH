package littlejian.ext

import littlejian._

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

  def <(other: VarOr[Byte]): Goal = GoalNumRangeByte(self, None, Some(Boundary(other, false)))

  def <=(other: VarOr[Byte]): Goal = GoalNumRangeByte(self, None, Some(Boundary(other, true)))

  def >(other: VarOr[Byte]): Goal = GoalNumRangeByte(self, Some(Boundary(other, false)), None)

  def >=(other: VarOr[Byte]): Goal = GoalNumRangeByte(self, Some(Boundary(other, true)), None)
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

  def <(other: VarOr[Short]): Goal = GoalNumRangeShort(self, None, Some(Boundary(other, false)))

  def <=(other: VarOr[Short]): Goal = GoalNumRangeShort(self, None, Some(Boundary(other, true)))

  def >(other: VarOr[Short]): Goal = GoalNumRangeShort(self, Some(Boundary(other, false)), None)

  def >=(other: VarOr[Short]): Goal = GoalNumRangeShort(self, Some(Boundary(other, true)), None)
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

  def <(other: VarOr[Int]): Goal = GoalNumRangeInt(self, None, Some(Boundary(other, false)))

  def <=(other: VarOr[Int]): Goal = GoalNumRangeInt(self, None, Some(Boundary(other, true)))

  def >(other: VarOr[Int]): Goal = GoalNumRangeInt(self, Some(Boundary(other, false)), None)

  def >=(other: VarOr[Int]): Goal = GoalNumRangeInt(self, Some(Boundary(other, true)), None)
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

  def <(other: VarOr[Long]): Goal = GoalNumRangeLong(self, None, Some(Boundary(other, false)))

  def <=(other: VarOr[Long]): Goal = GoalNumRangeLong(self, None, Some(Boundary(other, true)))

  def >(other: VarOr[Long]): Goal = GoalNumRangeLong(self, Some(Boundary(other, false)), None)

  def >=(other: VarOr[Long]): Goal = GoalNumRangeLong(self, Some(Boundary(other, true)), None)
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

  def <(other: VarOr[Float]): Goal = GoalNumRangeFloat(self, None, Some(Boundary(other, false)))

  def <=(other: VarOr[Float]): Goal = GoalNumRangeFloat(self, None, Some(Boundary(other, true)))

  def >(other: VarOr[Float]): Goal = GoalNumRangeFloat(self, Some(Boundary(other, false)), None)

  def >=(other: VarOr[Float]): Goal = GoalNumRangeFloat(self, Some(Boundary(other, true)), None)
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

  def <(other: VarOr[Double]): Goal = GoalNumRangeDouble(self, None, Some(Boundary(other, false)))

  def <=(other: VarOr[Double]): Goal = GoalNumRangeDouble(self, None, Some(Boundary(other, true)))

  def >(other: VarOr[Double]): Goal = GoalNumRangeDouble(self, Some(Boundary(other, false)), None)

  def >=(other: VarOr[Double]): Goal = GoalNumRangeDouble(self, Some(Boundary(other, true)), None)
}