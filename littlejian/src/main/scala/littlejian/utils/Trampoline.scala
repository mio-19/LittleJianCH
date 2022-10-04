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
    case more: TrampolineMore[_] => Trampoline {
      val tmp = more.call().asInstanceOf[Trampoline[T]]
      Trampoline {
        tmp.flatMap(f)
      }
    }
    case _ => f(self.asInstanceOf[T])
  }

  def map[U](f: T => U): Trampoline[U] = self match {
    case more: TrampolineMore[_] => Trampoline {
      val tmp = more.call().asInstanceOf[Trampoline[T]]
      Trampoline {
        tmp.map(f)
      }
    }
    case _ => f(self.asInstanceOf[T])
  }
}
