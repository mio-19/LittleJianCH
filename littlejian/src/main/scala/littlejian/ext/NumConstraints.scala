package littlejian.ext

import littlejian._

implicit class VarOrByteOps(self: VarOr[Byte]) {
  def +(other: VarOr[Byte]): Rel[Byte] = for {
    result <- fresh[Byte]
    _ <- GoalNumOpByte(NumOp2.Add, self, other, result)
  } yield result

  def -(other: VarOr[Byte]): Rel[Byte] = for {
    result <- fresh[Byte]
    _ <- GoalNumOpByte(NumOp2.Sub, self, other, result)
  } yield result

  def *(other: VarOr[Byte]): Rel[Byte] = for {
    result <- fresh[Byte]
    _ <- GoalNumOpByte(NumOp2.Mul, self, other, result)
  } yield result
}

implicit class VarOrShortOps(self: VarOr[Short]) {
  def +(other: VarOr[Short]): Rel[Short] = for {
    result <- fresh[Short]
    _ <- GoalNumOpShort(NumOp2.Add, self, other, result)
  } yield result

  def -(other: VarOr[Short]): Rel[Short] = for {
    result <- fresh[Short]
    _ <- GoalNumOpShort(NumOp2.Sub, self, other, result)
  } yield result

  def *(other: VarOr[Short]): Rel[Short] = for {
    result <- fresh[Short]
    _ <- GoalNumOpShort(NumOp2.Mul, self, other, result)
  } yield result
}

implicit class VarOrIntOps(self: VarOr[Int]) {
  def +(other: VarOr[Int]): Rel[Int] = for {
    result <- fresh[Int]
    _ <- GoalNumOpInt(NumOp2.Add, self, other, result)
  } yield result

  def -(other: VarOr[Int]): Rel[Int] = for {
    result <- fresh[Int]
    _ <- GoalNumOpInt(NumOp2.Sub, self, other, result)
  } yield result

  def *(other: VarOr[Int]): Rel[Int] = for {
    result <- fresh[Int]
    _ <- GoalNumOpInt(NumOp2.Mul, self, other, result)
  } yield result
}

implicit class VarOrLongOps(self: VarOr[Long]) {
  def +(other: VarOr[Long]): Rel[Long] = for {
    result <- fresh[Long]
    _ <- GoalNumOpLong(NumOp2.Add, self, other, result)
  } yield result

  def -(other: VarOr[Long]): Rel[Long] = for {
    result <- fresh[Long]
    _ <- GoalNumOpLong(NumOp2.Sub, self, other, result)
  } yield result

  def *(other: VarOr[Long]): Rel[Long] = for {
    result <- fresh[Long]
    _ <- GoalNumOpLong(NumOp2.Mul, self, other, result)
  } yield result
}

implicit class VarOrFloatOps(self: VarOr[Float]) {
  def +(other: VarOr[Float]): Rel[Float] = for {
    result <- fresh[Float]
    _ <- GoalNumOpFloat(NumOp2.Add, self, other, result)
  } yield result

  def -(other: VarOr[Float]): Rel[Float] = for {
    result <- fresh[Float]
    _ <- GoalNumOpFloat(NumOp2.Sub, self, other, result)
  } yield result

  def *(other: VarOr[Float]): Rel[Float] = for {
    result <- fresh[Float]
    _ <- GoalNumOpFloat(NumOp2.Mul, self, other, result)
  } yield result
}

implicit class VarOrDoubleOps(self: VarOr[Double]) {
  def +(other: VarOr[Double]): Rel[Double] = for {
    result <- fresh[Double]
    _ <- GoalNumOpDouble(NumOp2.Add, self, other, result)
  } yield result

  def -(other: VarOr[Double]): Rel[Double] = for {
    result <- fresh[Double]
    _ <- GoalNumOpDouble(NumOp2.Sub, self, other, result)
  } yield result

  def *(other: VarOr[Double]): Rel[Double] = for {
    result <- fresh[Double]
    _ <- GoalNumOpDouble(NumOp2.Mul, self, other, result)
  } yield result
}