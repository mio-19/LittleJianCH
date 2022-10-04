package littlejian.data

import littlejian._

import scala.language.implicitConversions

final implicit class Fix[F[_]](val out: F[Fix[F]]) extends Product1[F[Fix[F]]] {
  override def _1: F[Fix[F]] = out

  override def toString: String = out.toString

  override def hashCode: Int = out.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case that: Fix[_] => out == that.out
    case _ => out == obj
  }

  override def canEqual(that: Any): Boolean = that.equals(out)
}

implicit def U$Fix[F[_]](implicit U$F: Unifier[F[Fix[F]]]): Unifier[Fix[F]] = (((x: Any, y: Any) => (x, y) match {
  case (x: Fix[_], y: Fix[_]) => U$F.unify(x.asInstanceOf[Fix[F]].out, y.asInstanceOf[Fix[F]].out)
  case (x: Fix[_], y) => U$F.unify(x.asInstanceOf[Fix[F]].out, y.asInstanceOf[F[Fix[F]]])
  case (x, y: Fix[_]) => U$F.unify(x.asInstanceOf[F[Fix[F]]], y.asInstanceOf[Fix[F]].out)
  case (x, y) => U$F.unify(x.asInstanceOf[F[Fix[F]]], y.asInstanceOf[F[Fix[F]]])
}): Unifier[Any]).asInstanceOf[Unifier[Fix[F]]]
implicit def I$Fix[F[_]](implicit I$F: Inspector[F[Fix[F]]]): Inspector[Fix[F]] = (((x: Any) => x match {
  case x: Fix[_] => I$F.inspect(x.asInstanceOf[Fix[F]].out)
  case x => I$F.inspect(x.asInstanceOf[F[Fix[F]]])
}): Inspector[Any]).asInstanceOf[Inspector[Fix[F]]]

implicit def fix2unfix[F[_]](fix: Fix[F]): F[Fix[F]] = fix.out

implicit def unfix2fix[F[_]](unfix: F[Fix[F]]): Fix[F] = new Fix(unfix)

implicit def unfix2fixVarOr[F[_]](unfix: VarOr[F[Fix[F]]]): VarOr[Fix[F]] = unfix match {
  case v: Var[_] => v.asInstanceOf[VarOr[Fix[F]]]
  case out: F[Fix[F]] => new Fix(out)
}

implicit def fix2unfixVarOr[F[_]](fix: VarOr[Fix[F]]): VarOr[F[Fix[F]]] = fix match {
  case v: Var[_] => v.asInstanceOf[VarOr[F[Fix[F]]]]
  case out: Fix[F] => out.out
}