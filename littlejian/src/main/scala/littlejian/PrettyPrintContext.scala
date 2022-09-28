package littlejian

final class PrettyPrintContext(var subst: Subst) {
  var varCount: Int = 0
  var varNames: scala.collection.mutable.HashMap[Var[_], Int] = scala.collection.mutable.HashMap.empty

  // started from 1
  def nextVar: Int = {
    varCount += 1
    varCount
  }

  def getVar(x: Var[_]): Int = varNames.getOrElseUpdate(x, this.nextVar)
}

private var currentPrettyPrintContext = new java.lang.ThreadLocal[PrettyPrintContext]

object PrettyPrintContext {
  def callWith[T](context: PrettyPrintContext)(block: => T): T = {
    val previous = currentPrettyPrintContext.get()
    currentPrettyPrintContext.set(context)
    val result = block
    currentPrettyPrintContext.set(previous)
    result
  }

  def get: Option[PrettyPrintContext] = Option(currentPrettyPrintContext.get())
}
