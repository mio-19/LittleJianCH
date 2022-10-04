package littlejian

final case class EqState(subst: Subst) {
  override def toString: String = {
    prettyPrintContext.updateWith(_.disableSubst) {
      subst.map((v, entry) => s"${v}: ${entry._2}").mkString(", ")
    }
  }
}

object EqState {
  val empty: EqState = EqState(Subst.empty)
}

sealed class NotEqRequest[T](val x: VarOr[T], val y: VarOr[T], val unifier: Unifier[T]) {
  def relatedOnEq(v: Set[Var[_]]): Boolean = true
}

private def NotEqRequestUnchecked[T, U](x: T, y: U, unifier: Unifier[_]): NotEqRequest[_] =
  new NotEqRequest[T | U](x, y, unifier.asInstanceOf[Unifier[T | U]])

final case class NotEqElem[T](override val x: Var[T], override val y: VarOr[T], override val unifier: Unifier[T]) extends NotEqRequest[T](x, y, unifier) {
  override def relatedOnEq(v: Set[Var[_]]): Boolean = {
    val vs: Set[Any] = v.asInstanceOf[Set[Any]]
    vs.contains(x) || vs.contains(y)
  }

  override def toString: String = s"$x =/= $y"
}

object NotEqElem {
  def unchecked(x: Var[_], y: Any, unifier: Unifier[_]): NotEqElem[_] =
    NotEqElem(x.asInstanceOf[Var[Any]], y.asInstanceOf[VarOr[Any]], unifier.asInstanceOf[Unifier[Any]])
}

final case class NotEqState(clauses: Vector /*conj*/ [Vector[NotEqElem[_]] /*disj not eq*/ ]) {
  def onEq(eq: EqState, updatedVars: /*nullable*/ Set[Var[_]]): Option[NotEqState] =
    if (clauses.isEmpty) Some(this) else // optimize
      NotEqState.check(eq, clauses, updatedVars)

  def print: String = if (clauses.isEmpty) "" else clauses.seq.map(_.seq.map(_.toString).mkString(" || ")).mkString("\n")
}

object NotEqState {
  val empty: NotEqState = NotEqState(Vector.empty)

  import littlejian.utils._

  def relatedOnEq(v: Set[Var[_]], xs: Seq[NotEqRequest[_]]): Boolean = xs.exists(_.relatedOnEq(v))

  private def execCheck[T](eq: EqState, req: NotEqRequest[T], updatedVars: /*nullable*/ Set[Var[_]]): Option[Vector /*disj, empty means success*/ [NotEqElem[_]]] =
    if (updatedVars != null && !req.relatedOnEq(updatedVars)) Some(Vector(req.asInstanceOf[NotEqElem[_]]))
    else (eq.subst.walk(req.x), eq.subst.walk(req.y)) match {
      case (x: Var[T], y: Var[T]) if x == y => None
      case (x: Var[T], y) => Some(Vector(NotEqElem(x, y, req.unifier)))
      case (x, y: Var[T]) => Some(Vector(NotEqElem(y, x, req.unifier)))
      case (x, y) => req.unifier.unify(x, y).getSubstPatch(eq.subst) match {
        case None => Some(Vector.empty)
        case Some(newSubst) => if (newSubst.isEmpty) None else Some(newSubst.map({ case (v, unifier, x) => NotEqElem.unchecked(v, x, unifier) }))
      }
    }

  private def runCheck(eq: EqState, x: Seq /*disj*/ [NotEqRequest[_]], updatedVars: /*nullable*/ Set[Var[_]]): Option[Vector /*disj, empty means success*/ [NotEqElem[_]]] =
    if (x.isEmpty) throw new IllegalArgumentException("Empty vector")
    else if (updatedVars != null && !relatedOnEq(updatedVars, x)) Some(x.asInstanceOf[Seq[NotEqElem[_]]].toVector)
    else {
      val result = x.map(execCheck(eq, _, updatedVars)).filter(_.isDefined).map(_.get)
      if (result.isEmpty) None
      else if (result.exists(_.isEmpty)) Some(Vector.empty)
      else Some(result.fold(Vector.empty)(_ ++ _))
    }

  private def check(eq: EqState, xs: Vector[Vector[NotEqRequest[_]]], updatedVars: /*nullable*/ Set[Var[_]] = null): Option[NotEqState] =
    traverse(xs.map(runCheck(eq, _, updatedVars))).map(xs => NotEqState(xs.filter(_.nonEmpty)))

  private[littlejian] def insert(eq: EqState, req: NotEqRequest[_], clauses: NotEqState): Option[NotEqState] = check(eq, Vector(req) +: clauses.clauses)
}

final case class PredTypeState(xs: Vector[(Var[_], PredTypeTag)]) {
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

  def print: String = if (xs.isEmpty) "" else xs.map(x => s"${x._1}.isType[${x._2}]").mkString("\n")
}

object PredTypeState {
  val empty: PredTypeState = PredTypeState(Vector.empty)
}

final case class PredNotTypeState(xs: Vector[(Var[_], PredTypeTag)]) {
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

  def print: String = if (xs.isEmpty) "" else xs.map(x => s"${x._1}.isNotType[${x._2}]").mkString("\n")
}

object PredNotTypeState {
  val empty: PredNotTypeState = PredNotTypeState(Vector.empty)
}

final case class AbsentState(absents: Vector /*conj*/ [(Any, Vector /*disj*/ [WithInspector[_]])]) {
  def insert(eq: EqState, goal: GoalAbsent[_]): Option[AbsentState] = Inspector.scanUncertain(goal.x, eq.subst.walk, goal.absent) match {
    case None => None
    case Some(xs) => if (xs.isEmpty) Some(this) else Some(AbsentState((goal.absent, Vector.from(xs)) +: absents))
  }

  def onEq(eq: EqState): Option[AbsentState] = AbsentState.check(eq, absents)

  def print: String = if (absents.isEmpty) "" else absents.map(x => x._2.map(wi => s"${x._1}.absent(${wi.x})").mkString(" || ")).mkString("\n")
}

object AbsentState {
  val empty: AbsentState = AbsentState(Vector.empty)

  import littlejian.utils._

  def check(eq: EqState, absents: Vector /*conj*/ [(Any, Vector /*disj*/ [WithInspector[_]])]): Option[AbsentState] =
    if (absents.isEmpty) Some(AbsentState.empty) // optimize
    else {
      traverse(absents.map({ case (v, xs) => for {
        ys <- Inspector.scanUncertain(xs, eq.subst.walk, v)
      } yield (v, ys)
      })).map(
        next => AbsentState(next.filter({ case (_, xs) => xs.nonEmpty }))
      )
    }
}

final case class State(eq: EqState, notEq: NotEqState, predType: PredTypeState, predNotType: PredNotTypeState, absent: AbsentState) {
  def eqUpdated(eq: EqState): State = State(eq = eq, notEq = notEq, predType = predType, predNotType = predNotType, absent = absent)

  def notEqUpdated(notEq: NotEqState): State = State(eq = eq, notEq = notEq, predType = predType, predNotType = predNotType, absent = absent)

  def predTypeUpdated(predType: PredTypeState): State = State(eq = eq, notEq = notEq, predType = predType, predNotType = predNotType, absent = absent)

  def predNotTypeUpdated(predNotType: PredNotTypeState): State = State(eq = eq, notEq = notEq, predType = predType, predNotType = predNotType, absent = absent)

  def absentUpdated(absent: AbsentState): State = State(eq = eq, notEq = notEq, predType = predType, predNotType = predNotType, absent = absent)

  def predTypeMap(f: PredTypeState => PredTypeState): State = predTypeUpdated(f(predType))

  def predNotTypeMap(f: PredNotTypeState => PredNotTypeState): State = predNotTypeUpdated(f(predNotType))

  // Update Constraints
  def onEq(updatedVars: Set[Var[_]] = null): Option[State] = for {
    notEq <- notEq.onEq(eq, updatedVars)
    predType <- predType.onEq(eq)
    predNotType <- predNotType.onEq(eq)
    absent <- absent.onEq(eq)
  } yield State(eq = eq, notEq = notEq, predType = predType, predNotType = predNotType, absent = absent)

  def setEq(eq: EqState, updatedVars: Set[Var[_]] = null): Option[State] = this.eqUpdated(eq).onEq(updatedVars)

  def printConstraints: String = Vector(
    notEq.print,
    predType.print,
    predNotType.print,
    absent.print
  ).filter(_.nonEmpty).mkString("\n")

  override def toString: String = prettyPrintContext.callWithOrUpdate(new PrettyPrintContext(eq.subst), _.setSubst(eq.subst)) {
    Vector(eq.toString, printConstraints).filter(_.nonEmpty).mkString("\n")
  }
}

object State {
  val empty: State = State(eq = EqState.empty, notEq = NotEqState.empty, predType = PredTypeState.empty, predNotType = PredNotTypeState.empty, absent = AbsentState.empty)
}