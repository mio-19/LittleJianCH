package littlejian

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

final case class Boundary[T](x: T, eq: Boolean)

sealed trait GoalNumRange extends GoalBasic {
  def tag: NumTag

  def low: Option[Boundary[_ <: Num | Var[_ <: Num]]]

  def high: Option[Boundary[_ <: Num | Var[_ <: Num]]]

  override def execute(state: State): IterableOnce[State] = ???

  def walk(subst: Subst): GoalNumRange
}

final case class GoalNumRangeByte(low: Option[Boundary[VarOr[Byte]]], high: Option[Boundary[VarOr[Byte]]]) extends GoalNumRange {
  override def tag = NumTag.Byte

  override def walk(subst: Subst): GoalNumRangeByte = copy(low = low.map(x=>x.copy(x=subst.walk(x.x))), high = high.map(x=>x.copy(x=subst.walk(x.x))))
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
  def check: Vector[Unifying[Option[GoalNumRange]]] = ???
}

final case class NumState(op2s: Vector[GoalNumOp2], ranges: Vector[GoalNumRange]) {
  def insert(state: State, x: GoalNumOp2): IterableOnce[State] = copy(op2s = x +: op2s).onInsert(state)

  def insert(state: State, x: GoalNumRange): IterableOnce[State] = copy(ranges = x +: ranges).onInsert(state)

  def onEq(eq: EqState): IterableOnce[(EqState, NumState)] = for {
    (subst, ranges) <- NumState.runRanges(eq.subst, ranges)
    (subst, op2s) <- NumState.runOp2s(subst, op2s)
  } yield (EqState(subst), NumState(op2s = op2s, ranges = ranges))

  def onInsert(state: State): IterableOnce[State] = this.onEq(state.eq) map {
    case (eq, num) => state.copy(eq = eq, num = num)
  }

  def print: String = op2s.map(_.toString).mkString(" && ")
}

object NumState {
  def runOp2s(subst: Subst, op2s: Vector[GoalNumOp2]): Option[(Subst, Vector[GoalNumOp2])] = if (op2s.isEmpty) Some(subst, op2s) else // optimize
  {
    val (cl2, rest) = op2s.map(_.walk(subst)).partition(_.is2)
    Unifying.runAll(cl2.map(_.solve2)).getSubst(subst) map { subst =>
      (subst, rest)
    }
  }

  def runRanges(subst: Subst, ranges: Vector[GoalNumRange]): Option[(Subst, Vector[GoalNumRange])] = if (ranges.isEmpty) Some(subst, ranges) else // optimize
  {
    ???
  }

  val empty: NumState = NumState(Vector.empty, Vector.empty)
}
