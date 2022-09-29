package littlejian

final class Parameter[T] {
  private val current = new java.lang.ThreadLocal[T]

  def callWith[U](x: T)(block: => U): U = {
    val previous = current.get()
    current.set(x)
    val result = block
    current.set(previous)
    result
  }

  def get: Option[T] = Option(current.get())
}