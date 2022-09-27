package littlejian

final case class UnifiableBox[T](x: VarOr[T], unifier: Unifier[T])

// Monad
type Unifying[T] = Subst => Option[(Subst, T)]

implicit class UnifyingOps[T](self: Unifying[T]) {
  def map[U](f: T => U): Unifying[U] = subst => self(subst) match {
    case Some((s, x)) => Some((s, f(x)))
    case None => None
  }
  def flatMap[U](f: T => Unifying[U]): Unifying[U] = subst => self(subst) match {
    case Some((s, x)) => f(x)(s)
    case None => None
  }
}

trait Unifier[T] {
  final implicit val thisUnifier: Unifier[T] = this

  final def unify(self: VarOr[T], other: VarOr[T]): Unifying[Unit] = for {
    selfpak <- Subst.walk(self)
    UnifiableBox(self, unifier) = selfpak
    otherpak <- Subst.walk(other)(unifier)
    UnifiableBox(other, _) = otherpak
    _ <- (self, other) match {
      case (self: Var[_], _) => Subst.addEntry(self.asInstanceOf[Var[T]], otherpak)
      case (_, other: Var[_]) => Subst.addEntry(other.asInstanceOf[Var[T]], selfpak)
      case _ => concreteUnify(self.asInstanceOf[T], other.asInstanceOf[T])
    }
  } yield ()

  def concreteUnify(self: T, other: T): Unifying[Unit] = ???
}

implicit object UnifiableBoxUnifier extends Unifier[UnifiableBox[_]]