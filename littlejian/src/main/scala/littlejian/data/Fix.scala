package littlejian.data

import littlejian._

import scala.language.implicitConversions


type Fix0[F[_]] = F[F[F[F[Any]]]]

type Fix[F[_]] = F[Fix0[F]]

object FixHelperAny {
  implicit def U$Fix[F[_]](implicit U$F: Unifier[F[Fix0[F]]]): Unifier[Fix0[F]] = U$F.asInstanceOf[Unifier[Fix0[F]]]

  implicit def I$Fix[F[_]](implicit I$F: Inspector[F[Fix0[F]]]): Inspector[Fix0[F]] = I$F.asInstanceOf[Inspector[Fix0[F]]]

  implicit def fix2unfix[F[_]](fix: Fix0[F]): F[Fix0[F]] = fix.asInstanceOf[F[Fix0[F]]]

  implicit def unfix2fix[F[_]](unfix: F[Fix0[F]]): Fix0[F] = unfix.asInstanceOf[Fix0[F]]

  implicit def unfix2fixVarOr[F[_]](unfix: VarOr[F[Fix0[F]]]): VarOr[Fix0[F]] = unfix.asInstanceOf[VarOr[Fix0[F]]]

  implicit def fix2unfixVarOr[F[_]](fix: VarOr[Fix0[F]]): VarOr[F[Fix0[F]]] = fix.asInstanceOf[VarOr[F[Fix0[F]]]]

}


trait FixHelper[F[_]] {
  implicit def U$Fix(implicit U$F: Unifier[F[Fix0[F]]]): Unifier[Fix0[F]] = U$F.asInstanceOf[Unifier[Fix0[F]]]
  implicit def I$Fix(implicit I$F: Inspector[F[Fix0[F]]]): Inspector[Fix0[F]] = I$F.asInstanceOf[Inspector[Fix0[F]]]

  implicit def fix2unfix(fix: Fix0[F]): F[Fix0[F]] = fix.asInstanceOf[F[Fix0[F]]]

  implicit def unfix2fix(unfix: F[Fix0[F]]): Fix0[F] = unfix.asInstanceOf[Fix0[F]]

  implicit def unfix2fixVarOr(unfix: VarOr[F[Fix0[F]]]): VarOr[Fix0[F]] = unfix.asInstanceOf[VarOr[Fix0[F]]]

  implicit def fix2unfixVarOr(fix: VarOr[Fix0[F]]): VarOr[F[Fix0[F]]] = fix.asInstanceOf[VarOr[F[Fix0[F]]]]
}
