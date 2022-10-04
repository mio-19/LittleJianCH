package littlejian.data

import littlejian._

import scala.language.implicitConversions


type TypeRec[F[_]] = F[F[F[F[F[F[F[F[Any]]]]]]]]

object TypeRecHelperAny {
  implicit def U$Fix[F[_]](implicit U$F: Unifier[F[TypeRec[F]]]): Unifier[TypeRec[F]] = U$F.asInstanceOf[Unifier[TypeRec[F]]]

  implicit def I$Fix[F[_]](implicit I$F: Inspector[F[TypeRec[F]]]): Inspector[TypeRec[F]] = I$F.asInstanceOf[Inspector[TypeRec[F]]]

  implicit def fix2unfix[F[_]](fix: TypeRec[F]): F[TypeRec[F]] = fix.asInstanceOf[F[TypeRec[F]]]

  implicit def unfix2fix[F[_]](unfix: F[TypeRec[F]]): TypeRec[F] = unfix.asInstanceOf[TypeRec[F]]

  implicit def unfix2fixVarOr[F[_]](unfix: VarOr[F[TypeRec[F]]]): VarOr[TypeRec[F]] = unfix.asInstanceOf[VarOr[TypeRec[F]]]

  implicit def fix2unfixVarOr[F[_]](fix: VarOr[TypeRec[F]]): VarOr[F[TypeRec[F]]] = fix.asInstanceOf[VarOr[F[TypeRec[F]]]]

}


trait TypeRecHelper[F[_]] {
  implicit def U$Fix(implicit U$F: Unifier[F[TypeRec[F]]]): Unifier[TypeRec[F]] = U$F.asInstanceOf[Unifier[TypeRec[F]]]
  implicit def I$Fix(implicit I$F: Inspector[F[TypeRec[F]]]): Inspector[TypeRec[F]] = I$F.asInstanceOf[Inspector[TypeRec[F]]]

  implicit def fix2unfix(fix: TypeRec[F]): F[TypeRec[F]] = fix.asInstanceOf[F[TypeRec[F]]]

  implicit def unfix2fix(unfix: F[TypeRec[F]]): TypeRec[F] = unfix.asInstanceOf[TypeRec[F]]

  implicit def unfix2fixVarOr(unfix: VarOr[F[TypeRec[F]]]): VarOr[TypeRec[F]] = unfix.asInstanceOf[VarOr[TypeRec[F]]]

  implicit def fix2unfixVarOr(fix: VarOr[TypeRec[F]]): VarOr[F[TypeRec[F]]] = fix.asInstanceOf[VarOr[F[TypeRec[F]]]]
}
