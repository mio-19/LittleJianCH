package littlejian.utils

private val UsageLimit = 5

// TODO: Use this for subst for gc
final class ImmutableWeakHashMap[K, V](var prefix: scala.collection.immutable.HashMap[K, V],
                                       var core: scala.collection.mutable.WeakHashMap[K, V]) extends scala.collection.immutable.AbstractMap[K, V] {
  var usageCount = 0

  private def checkUsage: Unit = {
    if (prefix == null) return
      usageCount += 1
      if (usageCount >= UsageLimit) {
        this.synchronized {
          if (prefix != null) {
            // TODO
          }
        }
      }
  }

  def iterator: Iterator[(K, V)] = ???

  def get(key: K): Option[V] = {
    this.checkUsage
    this.synchronized {
      if (prefix == null) return core.get(key)
      prefix.get(key).orElse(core.get(key))
    }
  }

  def removed(key: K): Map[K, V] = ???

  def updated[V1 >: V](key: K, value: V1): Map[K, V1] = ???
}
