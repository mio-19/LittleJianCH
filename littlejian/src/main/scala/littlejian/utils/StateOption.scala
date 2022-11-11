package littlejian.utils

private object DeprecatedStateOption {

  // Monad
  type StateOption[State, T] = State => Trampoline[Option[(State, T)]]

  object StateOption {
    def success[State, T](t: T): StateOption[State, T] = s => Some(s, t)

    def failure[S, T]: StateOption[S, T] = _ => None

    def guard[S](x: Boolean): StateOption[S, Unit] = if (x) s => Some(s, ()) else failure

    def apply[State, T](f: State => Option[(State, T)]): StateOption[State, T] = f
  }

  implicit class StateOptionOps[S, T](self: StateOption[S, T]) {
    def run(state: S): Option[(S, T)] = self(state).get

    def map[U](f: T => U): StateOption[S, U] = (state: S) => self(state) flatMap {
      case Some((state, x)) => Trampoline(Some((state, f(x))))
      case None => None
    }

    def flatMap[U](f: T => StateOption[S, U]): StateOption[S, U] = (state: S) => self(state) flatMap {
      case Some((state, x)) => Trampoline(f.apply(x)(state))
      case None => None
    }

    def >>[U](other: StateOption[S, U]): StateOption[S, U] = self.flatMap(_ => other)
  }

}

private final class StateVar[T](var value: T)

final class StateOption[S, +T](val fn: StateVar[S] => Trampoline[Option[T]]) {
  def run(state: S): Option[(S, T)] = {
    val stateVar = new StateVar(state)
    fn(stateVar).get.map { t => (stateVar.value, t) }
  }

  inline def map[U](f: T => U): StateOption[S, U] = new StateOption(s => fn(s).map(_.map(f)))

  inline def flatMap[U](f: T => StateOption[S, U]): StateOption[S, U] = new StateOption(s => fn(s).flatMap {
    case Some(t) => Trampoline(f(t).fn(s))
    case None => None
  })

  inline def >>[U](other: StateOption[S, U]): StateOption[S, U] = flatMap(_ => other)
}

object StateOption {
  inline def success[S, T](t: T): StateOption[S, T] = new StateOption(_ => Some(t))

  inline def failure[S, T]: StateOption[S, T] = new StateOption(_ => None)

  inline def guard[S](x: Boolean): StateOption[S, Unit] = if (x) success(()) else failure

  inline def apply[S, T](f: S => Option[(S, T)]): StateOption[S, T] =
    new StateOption(s => f(s.value).match {
      case Some((s1, t)) => s.value = s1; Some(t)
      case None => None
    })
}