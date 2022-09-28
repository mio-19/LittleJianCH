package littlejian

final class PrettyPrintContext(subst: Subst) {
  var varCount: Int = 0
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
