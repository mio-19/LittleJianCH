package littlejian

import scala.collection.parallel.immutable.{ParHashMap, ParVector}

final case class EqState(subst: Subst)

object EqState {
  val empty: EqState = EqState(Subst.empty)
}

sealed class NotEqRequest[T](val x: VarOr[T], val y: VarOr[T], val unifier: Unifier[T])

final case class NotEqElem[T](override val x: Var[T], override val y: VarOr[T], override val unifier: Unifier[T]) extends NotEqRequest[T](x, y, unifier)

final case class NotEqState(clauses: ParVector /*conj*/ [ParVector[NotEqElem[_]] /*disj not eq*/ ]) {
  def onEq(eq: EqState): Option[NotEqState] =
    if (clauses.isEmpty) Some(this) else // optimize
      NotEqState.create(eq, clauses)
}

object NotEqState {
  val empty: NotEqState = NotEqState(ParVector.empty)

  private[littlejian] def create(eq: EqState, req: NotEqRequest[_], clauses: NotEqState): Option[NotEqState] = ???

  private[littlejian] def create(eq: EqState, clauses: ParVector[ParVector[NotEqElem[_]]]): Option[NotEqState] = ???
}

final case class PredTypeState(xs: ParVector[(Var[_], PredTypeTag)]) {
  def insert(v: Var[_], t: PredTypeTag): PredTypeState = PredTypeState((v, t) +: xs)

  def onEq(eq: EqState): Option[PredTypeState] = {
    if (xs.isEmpty) return Some(this) // optimize
    val (bound0, rest) = xs.partition(x => eq.subst.contains(x._1))
    val bound = bound0.map(x => (eq.subst.walk(x._1), x._2))
    val (vars, concretes) = bound.partition(x => x._1 match {
      case _: Var[_] => true
      case _ => false
    })
    if (concretes.forall(x => checkPredTypeTag(x._2, x._1))) Some(PredTypeState(vars.map(x => (x._1.asInstanceOf[Var[_]], x._2)) ++ rest)) else None
  }
}

object PredTypeState {
  val empty: PredTypeState = PredTypeState(ParVector.empty)
}

final case class PredNotTypeState(xs: ParVector[(Var[_], PredTypeTag)]) {
  def insert(v: Var[_], t: PredTypeTag): PredNotTypeState = PredNotTypeState((v, t) +: xs)

  def onEq(eq: EqState): Option[PredNotTypeState] = {
    if (xs.isEmpty) return Some(this) // optimize
    val (bound0, rest) = xs.partition(x => eq.subst.contains(x._1))
    val bound = bound0.map(x => (eq.subst.walk(x._1), x._2))
    val (vars, concretes) = bound.partition(x => x._1 match {
      case _: Var[_] => true
      case _ => false
    })
    if (concretes.forall(x => !checkPredTypeTag(x._2, x._1))) Some(PredNotTypeState(vars.map(x => (x._1.asInstanceOf[Var[_]], x._2)) ++ rest)) else None
  }
}

object PredNotTypeState {
  val empty: PredNotTypeState = PredNotTypeState(ParVector.empty)
}

final case class State(eq: EqState, notEq: NotEqState, predType: PredTypeState, predNotType: PredNotTypeState) {
  def eqUpdated(eq: EqState): State = State(eq = eq, notEq = notEq, predType = predType, predNotType = predNotType)

  def notEqUpdated(notEq: NotEqState): State = State(eq = eq, notEq = notEq, predType = predType, predNotType = predNotType)

  def predTypeMap(f: PredTypeState => PredTypeState): State = State(eq = eq, notEq = notEq, predType = f(predType), predNotType = predNotType)

  def predNotTypeMap(f: PredNotTypeState => PredNotTypeState): State = State(eq = eq, notEq = notEq, predType = predType, predNotType = f(predNotType))

  // Update Constraints
  def onEq: Option[State] = for {
    notEq <- notEq.onEq(eq)
    predType <- predType.onEq(eq)
    predNotType <- predNotType.onEq(eq)
  } yield State(eq = eq, notEq = notEq, predType = predType, predNotType = predNotType)

  def setEq(eq: EqState) = this.eqUpdated(eq).onEq
}

object State {
  val empty: State = State(eq = EqState.empty, notEq = NotEqState.empty, predType = PredTypeState.empty, predNotType = PredNotTypeState.empty)
}