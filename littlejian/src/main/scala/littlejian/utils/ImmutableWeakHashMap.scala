package littlejian.utils

private val PrefixLimit = 16

// TODO: Use this for subst for gc
final class ImmutableWeakHashMap[K, V](private[utils] val prefix: scala.collection.immutable.HashMap[K, _ <: V],
                                       private[utils] val core: scala.collection.mutable.WeakHashMap[K, _ <: V]) extends scala.collection.immutable.AbstractMap[K, V] {

  def iterator: Iterator[(K, V)] = prefix.iterator ++ core.iterator.filter({ case (k, _) => !prefix.contains(k) })

  def get(key: K): Option[V] = prefix.get(key).orElse(core.get(key))

  def removed(key: K): Map[K, V] = ???

  def updated[V1 >: V](key: K, value: V1): Map[K, V1] = ImmutableWeakHashMap.create(prefix.updated(key, value), core)
}

object ImmutableWeakHashMap {
  def empty[K, V]: ImmutableWeakHashMap[K, V] = new ImmutableWeakHashMap[K, V](scala.collection.immutable.HashMap.empty, new scala.collection.mutable.WeakHashMap[K, V])

  private[utils] def create[K, V](prefix: scala.collection.immutable.HashMap[K, _ <: V], core: scala.collection.mutable.WeakHashMap[K, _ <: V]): ImmutableWeakHashMap[K, V] =
    if (prefix.size > PrefixLimit)
      ???
    else
      new ImmutableWeakHashMap[K, V](prefix, core)
}
