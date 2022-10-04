package littlejian.data

import littlejian._

import scala.language.implicitConversions


type Fix[F[_]] = F[F[F[F[Any]]]]

object FixHelperAny {
  implicit def U$Fix[F[_]](implicit U$F: Unifier[F[Fix[F]]]): Unifier[Fix[F]] = U$F.asInstanceOf[Unifier[Fix[F]]]

  implicit def I$Fix[F[_]](implicit I$F: Inspector[F[Fix[F]]]): Inspector[Fix[F]] = I$F.asInstanceOf[Inspector[Fix[F]]]

  implicit def fix2unfix[F[_]](fix: Fix[F]): F[Fix[F]] = fix.asInstanceOf[F[Fix[F]]]

  implicit def unfix2fix[F[_]](unfix: F[Fix[F]]): Fix[F] = unfix.asInstanceOf[Fix[F]]

  implicit def unfix2fixVarOr[F[_]](unfix: VarOr[F[Fix[F]]]): VarOr[Fix[F]] = unfix.asInstanceOf[VarOr[Fix[F]]]

  implicit def fix2unfixVarOr[F[_]](fix: VarOr[Fix[F]]): VarOr[F[Fix[F]]] = fix.asInstanceOf[VarOr[F[Fix[F]]]]

}


trait FixHelper[F[_]] {
  implicit def U$Fix(implicit U$F: Unifier[F[Fix[F]]]): Unifier[Fix[F]] = U$F.asInstanceOf[Unifier[Fix[F]]]
  implicit def I$Fix(implicit I$F: Inspector[F[Fix[F]]]): Inspector[Fix[F]] = I$F.asInstanceOf[Inspector[Fix[F]]]

  implicit def fix2unfix(fix: Fix[F]): F[Fix[F]] = fix.asInstanceOf[F[Fix[F]]]

  implicit def unfix2fix(unfix: F[Fix[F]]): Fix[F] = unfix.asInstanceOf[Fix[F]]

  implicit def unfix2fixVarOr(unfix: VarOr[F[Fix[F]]]): VarOr[Fix[F]] = unfix.asInstanceOf[VarOr[Fix[F]]]

  implicit def fix2unfixVarOr(fix: VarOr[Fix[F]]): VarOr[F[Fix[F]]] = fix.asInstanceOf[VarOr[F[Fix[F]]]]
}
