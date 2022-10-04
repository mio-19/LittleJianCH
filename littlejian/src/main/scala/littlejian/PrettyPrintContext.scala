package littlejian

final class PrettyPrintVarCounter {
  var varCount: Int = 0
  var varNames: scala.collection.mutable.HashMap[Var[_], Int] = scala.collection.mutable.HashMap.empty

  // started from 1
  def nextVar: Int = {
    varCount += 1
    varCount
  }

  def getVar(x: Var[_]): Int = varNames.getOrElseUpdate(x, this.nextVar)


}

final class PrettyPrintContext(val subst: Subst, val counter: PrettyPrintVarCounter = new PrettyPrintVarCounter) {
  // started from 1
  def nextVar: Int = counter.nextVar

  def getVar(x: Var[_]): Int = counter.getVar(x)

  def disableSubst: PrettyPrintContext = new PrettyPrintContext(Subst.empty, this.counter)
  def setSubst(subst: Subst): PrettyPrintContext = new PrettyPrintContext(subst, this.counter)
}

val prettyPrintContext = new Parameter[PrettyPrintContext]
