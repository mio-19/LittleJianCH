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

final case class NumState(clauses: Vector[GoalNumOp]) {
  def insert(eq: EqState, x: GoalNumOp): IterableOnce[NumState] = Some(NumState(x +: clauses))

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