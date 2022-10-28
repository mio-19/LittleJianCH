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

sealed trait GoalNumOp extends GoalBasic {
  def rel: NumOp2

  def tag: NumTag

  def x: Num | Var[_ <: Num]

  def y: Num | Var[_ <: Num]

  def result: Num | Var[_ <: Num]

  override def execute(state: State): IterableOnce[State] = state.num.insert(state, this)

  def walk(subst: Subst): GoalNumOp

  override def toString: String = {
    val relName = rel match {
      case NumOp2.Add => "+"
      case NumOp2.Sub => "-"
      case NumOp2.Mul => "*"
    }
    s"$x $relName $y === $result"
  }
}

object GoalNumOp {
  def unapply(self: GoalNumOp): Some[(NumOp2, NumTag, Num | Var[_ <: Num], Num | Var[_ <: Num], Num | Var[_ <: Num])] = Some((self.rel, self.tag, self.x, self.y, self.result))
}

final case class GoalNumOpByte(rel: NumOp2, x: VarOr[Byte], y: VarOr[Byte], result: VarOr[Byte]) extends GoalNumOp {
  override def tag = NumTag.Byte

  override def walk(subst: Subst): GoalNumOpByte = GoalNumOpByte(rel, subst.walk(x), subst.walk(y), subst.walk(result))
}

final case class GoalNumOpShort(rel: NumOp2, x: VarOr[Short], y: VarOr[Short], result: VarOr[Short]) extends GoalNumOp {
  override def tag = NumTag.Short

  override def walk(subst: Subst): GoalNumOpShort = GoalNumOpShort(rel, subst.walk(x), subst.walk(y), subst.walk(result))
}

final case class GoalNumOpInt(rel: NumOp2, x: VarOr[Int], y: VarOr[Int], result: VarOr[Int]) extends GoalNumOp {
  override def tag = NumTag.Int

  override def walk(subst: Subst): GoalNumOpInt = GoalNumOpInt(rel, subst.walk(x), subst.walk(y), subst.walk(result))
}

final case class GoalNumOpLong(rel: NumOp2, x: VarOr[Long], y: VarOr[Long], result: VarOr[Long]) extends GoalNumOp {
  override def tag = NumTag.Long

  override def walk(subst: Subst): GoalNumOpLong = GoalNumOpLong(rel, subst.walk(x), subst.walk(y), subst.walk(result))
}

final case class GoalNumOpFloat(rel: NumOp2, x: VarOr[Float], y: VarOr[Float], result: VarOr[Float]) extends GoalNumOp {
  override def tag = NumTag.Float

  override def walk(subst: Subst): GoalNumOpFloat = GoalNumOpFloat(rel, subst.walk(x), subst.walk(y), subst.walk(result))
}

final case class GoalNumOpDouble(rel: NumOp2, x: VarOr[Double], y: VarOr[Double], result: VarOr[Double]) extends GoalNumOp {
  override def tag = NumTag.Double

  override def walk(subst: Subst): GoalNumOpDouble = GoalNumOpDouble(rel, subst.walk(x), subst.walk(y), subst.walk(result))
}

final case class NumState(clauses: Vector[GoalNumOp]) {
  def insert(state: State, x: GoalNumOp): IterableOnce[State] = NumState(x +: clauses).onEq(state.eq) map {
    case (eq, num) => state.eqUpdated(eq).numUpdated(num)
  }

  def onEq(eq: EqState): IterableOnce[(EqState, NumState)] = onEq(eq.subst) map {
    case (subst, num) => (EqState(subst), num)
  }

  def onEq(subst: Subst): Option[(Subst, NumState)] = {
    val (cl2, rest) = clauses.map(_.walk(subst)).partition(_.is2)
    Unifying.runAll(cl2.map(_.solve2)).getSubst(subst) map { subst =>
      (subst, NumState(rest))
    }
  }

  def print: String = clauses.map(_.toString).mkString(" && ")
}

implicit class GoalNumOpOps(self: GoalNumOp) {
  def is2: Boolean = {
    val a = if (self.x.isInstanceOf[Num]) 1 else 0
    val b = if (self.y.isInstanceOf[Num]) 1 else 0
    val c = if (self.result.isInstanceOf[Num]) 1 else 0
    a + b + c >= 2
  }

  def solve2: Unifying[Unit] = self match {
    case GoalNumOpByte(NumOp2.Add, x: Byte, y: Byte, rel) => rel.unify((x + y).asInstanceOf[Byte])
    case GoalNumOpByte(NumOp2.Add, x, y: Byte, rel: Byte) => x.unify((rel - y).asInstanceOf[Byte])
    case GoalNumOpByte(NumOp2.Add, x: Byte, y, rel: Byte) => y.unify((rel - x).asInstanceOf[Byte])
    case GoalNumOpByte(NumOp2.Sub, x: Byte, y: Byte, rel) => rel.unify((x - y).asInstanceOf[Byte])
    case GoalNumOpByte(NumOp2.Sub, x, y: Byte, rel: Byte) => x.unify((rel + y).asInstanceOf[Byte])
    case GoalNumOpByte(NumOp2.Sub, x: Byte, y, rel: Byte) => y.unify((x - rel).asInstanceOf[Byte])
    case GoalNumOpByte(NumOp2.Mul, x: Byte, y: Byte, rel) => rel.unify((x * y).asInstanceOf[Byte])
    case GoalNumOpByte(NumOp2.Mul, x, y: Byte, rel: Byte) => if (rel % y == 0) x.unify((rel / y).asInstanceOf[Byte]) else Unifying.failure
    case GoalNumOpByte(NumOp2.Mul, x: Byte, y, rel: Byte) => if (rel % x == 0) y.unify((rel / x).asInstanceOf[Byte]) else Unifying.failure
    case GoalNumOpByte(_, _, _, _) => throw new IllegalArgumentException("not a 2-arg goal")
    case GoalNumOpShort(NumOp2.Add, x: Short, y: Short, rel) => rel.unify((x + y).asInstanceOf[Short])
    case GoalNumOpShort(NumOp2.Add, x, y: Short, rel: Short) => x.unify((rel - y).asInstanceOf[Short])
    case GoalNumOpShort(NumOp2.Add, x: Short, y, rel: Short) => y.unify((rel - x).asInstanceOf[Short])
    case GoalNumOpShort(NumOp2.Sub, x: Short, y: Short, rel) => rel.unify((x - y).asInstanceOf[Short])
    case GoalNumOpShort(NumOp2.Sub, x, y: Short, rel: Short) => x.unify((rel + y).asInstanceOf[Short])
    case GoalNumOpShort(NumOp2.Sub, x: Short, y, rel: Short) => y.unify((x - rel).asInstanceOf[Short])
    case GoalNumOpShort(NumOp2.Mul, x: Short, y: Short, rel) => rel.unify((x * y).asInstanceOf[Short])
    case GoalNumOpShort(NumOp2.Mul, x, y: Short, rel: Short) => if (rel % y == 0) x.unify((rel / y).asInstanceOf[Short]) else Unifying.failure
    case GoalNumOpShort(NumOp2.Mul, x: Short, y, rel: Short) => if (rel % x == 0) y.unify((rel / x).asInstanceOf[Short]) else Unifying.failure
    case GoalNumOpShort(_, _, _, _) => throw new IllegalArgumentException("not a 2-arg goal")
    case GoalNumOpInt(NumOp2.Add, x: Int, y: Int, rel) => rel.unify(x + y)
    case GoalNumOpInt(NumOp2.Add, x, y: Int, rel: Int) => x.unify(rel - y)
    case GoalNumOpInt(NumOp2.Add, x: Int, y, rel: Int) => y.unify(rel - x)
    case GoalNumOpInt(NumOp2.Sub, x: Int, y: Int, rel) => rel.unify(x - y)
    case GoalNumOpInt(NumOp2.Sub, x, y: Int, rel: Int) => x.unify(rel + y)
    case GoalNumOpInt(NumOp2.Sub, x: Int, y, rel: Int) => y.unify(x - rel)
    case GoalNumOpInt(NumOp2.Mul, x: Int, y: Int, rel) => rel.unify(x * y)
    case GoalNumOpInt(NumOp2.Mul, x, y: Int, rel: Int) => if (rel % y == 0) x.unify(rel / y) else Unifying.failure
    case GoalNumOpInt(NumOp2.Mul, x: Int, y, rel: Int) => if (rel % x == 0) y.unify(rel / x) else Unifying.failure
    case GoalNumOpInt(_, _, _, _) => throw new IllegalArgumentException("not a 2-arg goal")
    case GoalNumOpLong(NumOp2.Add, x: Long, y: Long, rel) => rel.unify(x + y)
    case GoalNumOpLong(NumOp2.Add, x, y: Long, rel: Long) => x.unify(rel - y)
    case GoalNumOpLong(NumOp2.Add, x: Long, y, rel: Long) => y.unify(rel - x)
    case GoalNumOpLong(NumOp2.Sub, x: Long, y: Long, rel) => rel.unify(x - y)
    case GoalNumOpLong(NumOp2.Sub, x, y: Long, rel: Long) => x.unify(rel + y)
    case GoalNumOpLong(NumOp2.Sub, x: Long, y, rel: Long) => y.unify(x - rel)
    case GoalNumOpLong(NumOp2.Mul, x: Long, y: Long, rel) => rel.unify(x * y)
    case GoalNumOpLong(NumOp2.Mul, x, y: Long, rel: Long) => if (rel % y == 0) x.unify(rel / y) else Unifying.failure
    case GoalNumOpLong(NumOp2.Mul, x: Long, y, rel: Long) => if (rel % x == 0) y.unify(rel / x) else Unifying.failure
    case GoalNumOpLong(_, _, _, _) => throw new IllegalArgumentException("not a 2-arg goal")
    case GoalNumOpFloat(NumOp2.Add, x: Float, y: Float, rel) => rel.unify(x + y)
    case GoalNumOpFloat(NumOp2.Add, x, y: Float, rel: Float) => x.unify(rel - y)
    case GoalNumOpFloat(NumOp2.Add, x: Float, y, rel: Float) => y.unify(rel - x)
    case GoalNumOpFloat(NumOp2.Sub, x: Float, y: Float, rel) => rel.unify(x - y)
    case GoalNumOpFloat(NumOp2.Sub, x, y: Float, rel: Float) => x.unify(rel + y)
    case GoalNumOpFloat(NumOp2.Sub, x: Float, y, rel: Float) => y.unify(x - rel)
    case GoalNumOpFloat(NumOp2.Mul, x: Float, y: Float, rel) => rel.unify(x * y)
    case GoalNumOpFloat(NumOp2.Mul, x, y: Float, rel: Float) => x.unify(rel / y)
    case GoalNumOpFloat(NumOp2.Mul, x: Float, y, rel: Float) => y.unify(rel / x)
    case GoalNumOpFloat(_, _, _, _) => throw new IllegalArgumentException("not a 2-arg goal")
    case GoalNumOpDouble(NumOp2.Add, x: Double, y: Double, rel) => rel.unify(x + y)
    case GoalNumOpDouble(NumOp2.Add, x, y: Double, rel: Double) => x.unify(rel - y)
    case GoalNumOpDouble(NumOp2.Add, x: Double, y, rel: Double) => y.unify(rel - x)
    case GoalNumOpDouble(NumOp2.Sub, x: Double, y: Double, rel) => rel.unify(x - y)
    case GoalNumOpDouble(NumOp2.Sub, x, y: Double, rel: Double) => x.unify(rel + y)
    case GoalNumOpDouble(NumOp2.Sub, x: Double, y, rel: Double) => y.unify(x - rel)
    case GoalNumOpDouble(NumOp2.Mul, x: Double, y: Double, rel) => rel.unify(x * y)
    case GoalNumOpDouble(NumOp2.Mul, x, y: Double, rel: Double) => x.unify(rel / y)
    case GoalNumOpDouble(NumOp2.Mul, x: Double, y, rel: Double) => y.unify(rel / x)
    case GoalNumOpDouble(_, _, _, _) => throw new IllegalArgumentException("not a 2-arg goal")
  }
}

object NumState {
  val empty: NumState = NumState(Vector.empty)
}