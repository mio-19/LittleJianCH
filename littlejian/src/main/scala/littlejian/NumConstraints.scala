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
  case Div
  case Rem

sealed trait GoalNumOp extends GoalBasic {
  def rel: NumOp2

  def tag: NumTag

  def x: Num | Var[_ <: Num]

  def y: Num | Var[_ <: Num]

  def result: Num | Var[_ <: Num]

  override def execute(state: State): IterableOnce[State] = state.num.insert(state, this)
}

final case class GoalNumOpByte(rel: NumOp2, x: VarOr[Byte], y: VarOr[Byte], result: VarOr[Byte]) extends GoalNumOp {
  override def tag = NumTag.Byte
}

final case class GoalNumOpShort(rel: NumOp2, x: VarOr[Byte], y: VarOr[Byte], result: VarOr[Byte]) extends GoalNumOp {
  override def tag = NumTag.Short
}

final case class GoalNumOpInt(rel: NumOp2, x: VarOr[Byte], y: VarOr[Byte], result: VarOr[Byte]) extends GoalNumOp {
  override def tag = NumTag.Int
}

final case class GoalNumOpLong(rel: NumOp2, x: VarOr[Byte], y: VarOr[Byte], result: VarOr[Byte]) extends GoalNumOp {
  override def tag = NumTag.Long
}

final case class NumState(clauses: Vector[GoalNumOp]) {
  def insert(state: State, x: GoalNumOp): IterableOnce[State] = NumState(x +: clauses).onEq(state.eq) map {
    case (eq, num) => state.eqUpdated(eq).numUpdated(num)
  }

  def onEq(eq: EqState): IterableOnce[(EqState, NumState)] = Some((eq, this)) // TODO

  def print: String = "" // TODO
}

implicit class GoalNumOpOps(self: GoalNumOp) {
  def is2: Boolean = {
    val a = if(self.x.isInstanceOf[Num])  1 else 0
    val b = if(self.y.isInstanceOf[Num])  1 else 0
    val c = if(self.result.isInstanceOf[Num])  1 else 0
    a + b + c >= 2
  }

  def solve2: Unifying[Unit] = ???
}

object NumState {
  val empty: NumState = NumState(Vector.empty)
}