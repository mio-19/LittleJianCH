package littlejian

// started from 1
final class ToStringRecCatcherCounter {
  var count: Int = 0

  @inline def get: Int = {
    count = count + 1
    count
  }
}

final case class ToStringRecCatcherState(counter: ToStringRecCatcherCounter, history: scala.collection.immutable.HashSet[Any], notified: scala.collection.mutable.HashMap[Any, Int]) {
}

object ToStringRecCatcherState {
  @inline def empty: ToStringRecCatcherState = ToStringRecCatcherState(new ToStringRecCatcherCounter, scala.collection.immutable.HashSet.empty, new scala.collection.mutable.HashMap())
}

object ToStringRecCatcher {
  val parameter = new Parameter[ToStringRecCatcherState]

  type Result = String

  def record[T](self: T, recOuter: (Int, Result) => Result, recInner: Int => Result)(block: => Result): Result = {
    val stateOuter = parameter.get.getOrElse(ToStringRecCatcherState.empty)
    if (stateOuter.history.contains(self)) {
      val id = stateOuter.counter.get
      if (stateOuter.notified.contains(self)) {
        // TODO: are there some unhandled cases?
        throw new IllegalStateException("TODO")
      }
      stateOuter.notified.update(self, id)
      return recInner(id)
    }
    val stateInner = ToStringRecCatcherState(stateOuter.counter, stateOuter.history.incl(self), stateOuter.notified)
    if (stateOuter.notified.contains(self)) {
      // TODO: are there some unhandled cases?
      throw new IllegalStateException("TODO")
    }
    val result = parameter.callWith(stateInner) {
      block
    }
    if (stateOuter.notified.contains(self)) {
      val id = stateOuter.notified.apply(self)
      stateOuter.notified.remove(self)
      return recOuter(id, result)
    }
    result
  }
}
