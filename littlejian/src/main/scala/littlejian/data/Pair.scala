package littlejian.data

import littlejian._

final case class Pair[T, U](x: VarOr[T], y: VarOr[U]) extends Product2[VarOr[T], VarOr[U]] {
  private def tailToString: String = y match {
    case y: Pair[_, _] => s"$x ${y.tailToString}"
    case v: Var[_] => prettyPrintContext.get match {
      case Some(context) => context.subst.getOption(v) match {
        case Some(y) => Pair(x, y).tailToString
        case None => s"$x . $y)"
      }
    }
    case () => s"$x)"
    case _ => s"$x . $y)"
  }
  override def toString: String = s"(${tailToString}"
}

implicit def U$Pair[T, U](implicit U$T: Unifier[T], U$U: Unifier[U]): Unifier[Pair[T, U]] = U$Product(U$VarOr(U$T), U$VarOr(U$U))

implicit def I$Pair[T, U](implicit I$T: Inspector[T], I$U: Inspector[U]): Inspector[Pair[T, U]] = I$Product(I$VarOr(I$T), I$VarOr(I$U))