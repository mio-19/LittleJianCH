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

val prettyPrintContext = new Parameter[PrettyPrintContext]
