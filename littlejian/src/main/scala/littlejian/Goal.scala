package littlejian

import scala.annotation.{tailrec, targetName}

sealed trait Goal

sealed trait GoalBasic extends Goal {
  def execute(state: State): IterableOnce[State]
}

final case class GoalEq[T](x: VarOr[T], y: VarOr[T])(implicit unifier: Unify[T]) extends GoalBasic {
  override def execute(state: State): IterableOnce[State] = unifier.unify(x, y).getSubstWithPatch(state.eq.subst) match {
    case Some(subst, patch) => state.setEq(EqState(subst), Set.from(patch.map(_._1)))
    case None => None
  }

  override def toString: String = s"${x} === ${y}"
}

final case class GoalNotEq[T](x: VarOr[T], y: VarOr[T])(implicit unifier: Unify[T]) extends GoalBasic {
  override def execute(state: State): Option[State] =
    for {
      notEq <- NotEqState.insert(state.eq, new NotEqRequest(x, y, unifier), state.notEq)
    } yield state.notEqUpdated(notEq)

  override def toString: String = s"${x} =/= ${y}"
}

import scala.reflect.ClassTag

type PredTypeTag = ClassTag[_]

def checkPredTypeTag[T](tag: PredTypeTag, x: Any): Boolean = tag.runtimeClass.isInstance(x)

final case class GoalPredType[T](tag: PredTypeTag, x: VarOr[T]) extends GoalBasic {
  override def execute(state: State): Option[State] = state.eq.subst.walk(x) match {
    case v: Var[_] => Some(state.predTypeMap(_.insert(v, tag)))
    case x => if (checkPredTypeTag(tag, x)) Some(state) else None
  }

  override def toString: String = s"${x}.isType[${tag}]"
}

final case class GoalPredNotType[T](tag: PredTypeTag, x: VarOr[T]) extends GoalBasic {
  override def execute(state: State): Option[State] = state.eq.subst.walk(x) match {
    case v: Var[_] => Some(state.predNotTypeMap(_.insert(v, tag)))
    case x => if (!checkPredTypeTag(tag, x)) Some(state) else None
  }

  override def toString: String = s"${x}.isNotType[${tag}]"
}

final case class GoalAbsent[T](x: WithInspector[T], absent: Any) extends GoalBasic {
  override def execute(state: State): Option[State] = state.absent.insert(state.eq, this).map(state.absentUpdated)

  override def toString: String = s"${x.x}.absent(${absent})"
}

sealed trait GoalControl extends Goal

val goalDelayEvalLevel = new Parameter[Int]
val defaultGoalDelayEvalLevel = 3

final class GoalDelay(x: => Goal) extends GoalControl {
  def get: Goal = x

  @tailrec def forceN(n: Int): Goal = if (n <= 0) x else get match {
    case x: GoalDelay => x.forceN(n - 1)
    case x => x
  }

  override def toString: String = {
    val level = goalDelayEvalLevel.get.getOrElse(defaultGoalDelayEvalLevel)
    if (level <= 0) super.toString else goalDelayEvalLevel.callWith(level - 1) {
      this.get.toString
    }
  }
}

final case class GoalDisj(xs: Vector[Goal]) extends GoalControl {
  private def flatten: Vector[Goal] = xs.flatMap {
    case x: GoalDisj => x.flatten
    case v => Some(v)
  }

  override def toString: String = s"conde(${this.flatten.mkString(", ")})"
}

object GoalDisj {
  def apply(xs: Vector[Goal]) = new GoalDisj(xs)

  def apply(xs: Seq[Goal]) = new GoalDisj(Vector(xs *))

  @targetName("applyMul") def apply(xs: Goal*) = new GoalDisj(Vector(xs *))
}

final case class GoalConj(xs: Vector[Goal]) extends GoalControl {
  private def flatten: Vector[Goal] = xs.flatMap {
    case x: GoalConj => x.flatten
    case v => Some(v)
  }

  override def toString: String = s"begin(${this.flatten.mkString(", ")})"
}

final case class GoalReadSubst(f: Subst => Goal) extends GoalControl {
  def apply(subst: Subst): Goal = f(subst)
}

object GoalConj {
  def apply(xs: Vector[Goal]) = new GoalConj(xs)

  def apply(xs: Seq[Goal]) = new GoalConj(Vector.from(xs))

  @targetName("applyMul") def apply(xs: Goal*) = new GoalConj(Vector.from(xs))
}

sealed trait GoalControlImpure extends GoalControl

// conda
final case class GoalDisjA(xs: Vector[(Goal, Goal)]) extends GoalControlImpure {
  override def toString: String = s"conda(${xs.map({ case (test, goal) => s"(${test}, ${goal})" }) mkString (", ")})"
}

object GoalDisjA {
  def apply(xs: Seq[(Goal, Goal)]) = new GoalDisjA(Vector(xs *))
}

// condu
final case class GoalDisjU(xs: Vector[(Goal, Goal)]) extends GoalControlImpure {
  override def toString: String = s"condu(${xs.map({ case (test, goal) => s"(${test}, ${goal})" }) mkString (", ")})"
}

object GoalDisjU {
  def apply(xs: Seq[(Goal, Goal)]) = new GoalDisjU(Vector(xs *))
}

object Goal {
  val success: Goal = GoalConj(Vector())
  val failure: Goal = GoalDisj(Vector())

  def guard(x: Boolean): Goal = if (x) success else failure
}

// TODO: GoalFresh: capture fresh operators for the implementation of constructive negation
@inline def GoalFresh[T](f: Var[T] => Goal): Goal = f(new Var[T])

sealed trait GoalNumOp extends GoalBasic {
  def rel: NumOp2

  def tag: NumTag

  def x: Num | Var[_ <: Num]

  def y: Num | Var[_ <: Num]

  def result: Num | Var[_ <: Num]
}

final case class GoalNumOpByte(rel: NumOp2, x: VarOr[Byte], y: VarOr[Byte], result: VarOr[Byte]) extends GoalNumOp {
  override def tag = NumTag.Byte

  override def execute(state: State): IterableOnce[State] = ???
}