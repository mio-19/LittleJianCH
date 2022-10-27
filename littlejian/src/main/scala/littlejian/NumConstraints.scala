package littlejian

type Num = Byte | Short | Int | Long
implicit val U$Num: Unify[Num] = U$Union[Byte, Short, Int, Long]

enum NumOp2:
  case Add
  case Sub
  case Mul
  case Div
  case Rem

final case class NumState(clauses: Vector[GoalNumOp]) {
  def insert(eq: EqState, x: GoalNumOp): IterableOnce[NumState] = Some(NumState(x +: clauses)) // TODO

  def onEq(eq: EqState): IterableOnce[NumState] = Some(this) // TODO

  def print: String = "" // TODO
}

object NumState {
  val empty: NumState = NumState(Vector.empty)
}