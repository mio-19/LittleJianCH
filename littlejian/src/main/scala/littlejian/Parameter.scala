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
  
  def callWithOrUpdate[U](default: =>T, f: T => T)(block: => U): U = this.get match {
    case Some(x) => callWith(f(x))(block)
    case None => callWith(default)(block)
  }
  def updateWith[U](f: T => T)(block: => U): U = this.get match {
    case Some(x) => callWith(f(x))(block)
    case None => block
  }
  def defaultWith[U](default: =>T)(block: => U): U = this.get match {
    case Some(_) => block
    case None => callWith(default)(block)
  }
}