package littlejian.utils

import scala.collection.{MapFactory, mutable, immutable, IterableOnce}

private val PrefixLimit = 16

// TODO: Use this for subst for gc
final class ImmutableWeakHashMap[K, V] private(private[utils] val prefix: immutable.HashMap[K, _ <: V],
                                       private[utils] val core: mutable.WeakHashMap[K, _ <: V]) extends scala.collection.immutable.AbstractMap[K, V] {

  def iterator: Iterator[(K, V)] = prefix.iterator ++ core.iterator.filter({ case (k, _) => !prefix.contains(k) })

  def get(key: K): Option[V] = prefix.get(key).orElse(core.get(key))

  def removed(key: K): Map[K, V] =
    if (core.contains(key))
      ImmutableWeakHashMap.from(this.iterator.filter({ case (k, _) => k != key }))
    else new ImmutableWeakHashMap[K, V](prefix.removed(key), core)

  def updated[V1 >: V](key: K, value: V1): Map[K, V1] = ImmutableWeakHashMap.create(prefix.updated(key, value), core)
}

object ImmutableWeakHashMap extends MapFactory[ImmutableWeakHashMap] {
  def empty[K, V]: ImmutableWeakHashMap[K, V] = new ImmutableWeakHashMap[K, V](immutable.HashMap.empty, new mutable.WeakHashMap[K, V])

  private[utils] def create[K, V](prefix: immutable.HashMap[K, _ <: V], core: mutable.WeakHashMap[K, _ <: V]): ImmutableWeakHashMap[K, V] = {
    val result = new ImmutableWeakHashMap[K, V](prefix, core)
    if (prefix.size > PrefixLimit) {
      from(result)
    } else result
  }

  def from[K, V](it: IterableOnce[(K, V)]): ImmutableWeakHashMap[K, V] = new ImmutableWeakHashMap[K, V](immutable.HashMap.empty, mutable.WeakHashMap.from(it))

  def newBuilder[K, V]: mutable.Builder[(K, V), ImmutableWeakHashMap[K, V]] = ???
}
