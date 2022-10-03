package littlejian.utils

import scala.annotation.tailrec

type Trampoline[T] = TrampolineMore[T] | T

final case class TrampolineMore[T](call: () => Trampoline[T])

object Trampoline {
  @tailrec
  def get[T](t: Trampoline[T]): T = t match {
    case more: TrampolineMore[_] => get(more.call().asInstanceOf[Trampoline[T]])
    case _ => t.asInstanceOf[T]
  }
  def apply[T](t: => Trampoline[T]): Trampoline[T] = TrampolineMore(() => t)
}

implicit class TrampolineOps[T](self: Trampoline[T]) {
  def get: T = Trampoline.get(self)

  def flatMap[U](f: T => Trampoline[U]): Trampoline[U] = self match {
    case more: TrampolineMore[_] => TrampolineMore(() => more.call().asInstanceOf[Trampoline[T]].flatMap(f))
    case _ => f(self.asInstanceOf[T])
  }
}

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
