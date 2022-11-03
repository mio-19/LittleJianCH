package littlejian.data

import littlejian._
import littlejian.ext._

sealed trait Mapping[K, V] derives Unify {
  def get(k: K)(implicit uK: Unify[K], uV: Unify[V]): Rel[V]

  def getOption(k: K)(implicit uK: Unify[K], uV: Unify[V]): Rel[Option[V]]

  def notContains(k: K)(implicit uK: Unify[K]): Goal

  def updated(key: K, value: V): Mapping[K, V] = MappingNonEmpty(key, value, this)
}

final case class MappingEmpty[K, V]() extends Mapping[K, V] derives Unify {
  override def get(k: K)(implicit uK: Unify[K], uV: Unify[V]): Rel[V] = Rel.failure

  override def getOption(k: K)(implicit uK: Unify[K], uV: Unify[V]): Rel[Option[V]] = None

  override def notContains(k: K)(implicit uK: Unify[K]): Goal = Goal.success
}

final case class MappingNonEmpty[K, V](key: K, value: V, next: Mapping[K, V]) extends Mapping[K, V] derives Unify {
  override def get(k: K)(implicit uK: Unify[K], uV: Unify[V]): Rel[V] = compare(key, k) {
    value
  } {
    next.get(k)
  }

  override def getOption(k: K)(implicit uK: Unify[K], uV: Unify[V]): Rel[Option[V]] = compare(key, k) {
    Some(value)
  } {
    next.getOption(k)
  }

  override def notContains(k: K)(implicit uK: Unify[K]): Goal = compare(key, k) {
    Goal.failure
  } {
    next.notContains(k)
  }
}

object Mapping {
  def from[K, V](xs: Seq[(K, V)]): Mapping[K, V] = if (xs.isEmpty) MappingEmpty() else MappingNonEmpty(xs.head._1, xs.head._2, from(xs.tail))

  def apply[K, V](xs: (K, V)*): Mapping[K, V] = from(xs)
}