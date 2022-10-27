package littlejian

type Num = Byte | Short | Int | Long
implicit val U$Num: Unify[Num] = U$Union[Byte, Short, Int, Long]

enum NumOp2:
  case Add
  case Sub
  case Mul
  case Div
  case Rem

enum NumRelation:
  case Add
  case Mul
  case Div
  case Rem

final case class NumRelationClause(rel: NumRelation, lhs: VarOr[Num], rhs: VarOr[Num], result: VarOr[Num])

final case class NumState(clauses: Vector[NumRelationClause]) {
  def onEq(eq: EqState): Option[NumState] = Some(this)
  def print: String = "" // TODO
}

object NumState {
  val empty: NumState = NumState(Vector.empty)
}