package littlejian

final class PrettyPrintVarCounter {
  var varCount: Int = 0
  var varNames: scala.collection.mutable.HashMap[Var[_], Int] = scala.collection.mutable.HashMap.empty

  // started from 1
  inline def nextVar: Int = {
    varCount += 1
    varCount
  }

  def getVar(x: Var[_]): Int = varNames.getOrElseUpdate(x, this.nextVar)
}

final class PrettyPrintContext(val subst: Subst, val counter: PrettyPrintVarCounter = new PrettyPrintVarCounter) {
  // started from 1
  inline def nextVar: Int = counter.nextVar

  inline def getVar(x: Var[_]): Int = counter.getVar(x)

  inline def disableSubst: PrettyPrintContext = new PrettyPrintContext(Subst.empty, this.counter)
  inline def setSubst(subst: Subst): PrettyPrintContext = new PrettyPrintContext(subst, this.counter)
}

val prettyPrintContext = new Parameter[PrettyPrintContext]
