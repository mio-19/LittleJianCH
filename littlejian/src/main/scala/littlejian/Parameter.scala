package littlejian

final class Parameter[T] {
  private val current = new java.lang.ThreadLocal[T]

  inline def callWith[U](x: T)(block: => U): U = {
    val previous = current.get()
    current.set(x)
    val result = block
    current.set(previous)
    result
  }

  inline def get: Option[T] = Option(current.get())

  inline def callWithOrUpdate[U](default: =>T, f: T => T)(block: => U): U = this.get match {
    case Some(x) => callWith(f(x))(block)
    case None => callWith(default)(block)
  }
  inline def updateWith[U](f: T => T)(block: => U): U = this.get match {
    case Some(x) => callWith(f(x))(block)
    case None => block
  }
  inline def defaultWith[U](default: =>T)(block: => U): U = this.get match {
    case Some(_) => block
    case None => callWith(default)(block)
  }
}