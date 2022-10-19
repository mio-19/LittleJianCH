package littlejian

implicit def U$VarOr[T](implicit unifier: Unifier[T]): Unifier[VarOr[T]] = (x, y) => unifier.unify(x, y)
given unifierVarOr[T](using unifier: Unifier[T]): Unifier[VarOr[T]] = U$VarOr[T]


import scala.annotation.targetName
import scala.reflect.ClassTag

@targetName("U$Union_") def U$Union[T, U](implicit tr: => Unifier[T], ur: => Unifier[U], tev: ClassTag[T], uev: ClassTag[U]): Unifier[T | U] = U$Union(tr, ur)(tev, uev)

def U$Union[T, U](tr: => Unifier[T], ur: => Unifier[U])(implicit tev: ClassTag[T], uev: ClassTag[U]): Unifier[T | U] = {
  lazy val t = tr
  lazy val u = ur
  val tc = tev.runtimeClass
  val uc = uev.runtimeClass
  if (tc == uc) throw new IllegalArgumentException("T == U")
  (x, y) => {
    if (tc.isInstance(x) && tc.isInstance(y)) t.unify(x.asInstanceOf[T], y.asInstanceOf[T])
    else if (uc.isInstance(x) && uc.isInstance(y)) u.unify(x.asInstanceOf[U], y.asInstanceOf[U])
    else Unifying.failure
  }
}

@targetName("U$Union_") def U$Union[T, U, V](implicit tr: => Unifier[T], ur: => Unifier[U], vr: => Unifier[V], tev: ClassTag[T], uev: ClassTag[U], vev: ClassTag[V]): Unifier[T | U | V] = U$Union(tr, ur, vr)(tev, uev, vev)
def U$Union[T, U, V](tr: => Unifier[T], ur: => Unifier[U], vr: => Unifier[V])(implicit tev: ClassTag[T], uev: ClassTag[U], vev: ClassTag[V]): Unifier[T | U | V] = {
  lazy val t = tr
  lazy val u = ur
  lazy val v = vr
  val tc = tev.runtimeClass
  val uc = uev.runtimeClass
  val vc = vev.runtimeClass
  if (tc == uc || tc == vc || uc == vc) throw new IllegalArgumentException("T == U || T == V || U == V")
  (x, y) => {
    if (tc.isInstance(x) && tc.isInstance(y)) t.unify(x.asInstanceOf[T], y.asInstanceOf[T])
    else if (uc.isInstance(x) && uc.isInstance(y)) u.unify(x.asInstanceOf[U], y.asInstanceOf[U])
    else if (vc.isInstance(x) && vc.isInstance(y)) v.unify(x.asInstanceOf[V], y.asInstanceOf[V])
    else Unifying.failure
  }
}
@targetName("U$Union_") def U$Union[T, U, V, W](implicit tr: => Unifier[T], ur: => Unifier[U], vr: => Unifier[V], wr: => Unifier[W], tev: ClassTag[T], uev: ClassTag[U], vev: ClassTag[V], wev: ClassTag[W]): Unifier[T | U | V | W] = U$Union(tr, ur, vr, wr)(tev, uev, vev, wev)
def U$Union[T, U, V, W](tr: => Unifier[T], ur: => Unifier[U], vr: => Unifier[V], wr: => Unifier[W])(implicit tev: ClassTag[T], uev: ClassTag[U], vev: ClassTag[V], wev: ClassTag[W]): Unifier[T | U | V | W] = {
  lazy val t = tr
  lazy val u = ur
  lazy val v = vr
  lazy val w = wr
  val tc = tev.runtimeClass
  val uc = uev.runtimeClass
  val vc = vev.runtimeClass
  val wc = wev.runtimeClass
  if (tc == uc || tc == vc || tc == wc || uc == vc || uc == wc || vc == wc) throw new IllegalArgumentException("T == U || T == V || T == W || U == V || U == W || V == W")
  (x, y) => {
    if (tc.isInstance(x) && tc.isInstance(y)) t.unify(x.asInstanceOf[T], y.asInstanceOf[T])
    else if (uc.isInstance(x) && uc.isInstance(y)) u.unify(x.asInstanceOf[U], y.asInstanceOf[U])
    else if (vc.isInstance(x) && vc.isInstance(y)) v.unify(x.asInstanceOf[V], y.asInstanceOf[V])
    else if (wc.isInstance(x) && wc.isInstance(y)) w.unify(x.asInstanceOf[W], y.asInstanceOf[W])
    else Unifying.failure
  }
}
@targetName("U$Union_") def U$Union[A, B, C, D, E](implicit ar: => Unifier[A], br: => Unifier[B], cr: => Unifier[C], dr: => Unifier[D], er: => Unifier[E], aev: ClassTag[A], bev: ClassTag[B], cev: ClassTag[C], dev: ClassTag[D], eev: ClassTag[E]): Unifier[A | B | C | D | E] = U$Union(ar, br, cr, dr, er)(aev, bev, cev, dev, eev)
def U$Union[A, B, C, D, E](ar: => Unifier[A], br: => Unifier[B], cr: => Unifier[C], dr: => Unifier[D], er: => Unifier[E])(implicit aev: ClassTag[A], bev: ClassTag[B], cev: ClassTag[C], dev: ClassTag[D], eev: ClassTag[E]): Unifier[A | B | C | D | E] = {
  lazy val a = ar
  lazy val b = br
  lazy val c = cr
  lazy val d = dr
  lazy val e = er
  val ac = aev.runtimeClass
  val bc = bev.runtimeClass
  val cc = cev.runtimeClass
  val dc = dev.runtimeClass
  val ec = eev.runtimeClass
  if (ac == bc || ac == cc || ac == dc || ac == ec || bc == cc || bc == dc || bc == ec || cc == dc || cc == ec || dc == ec) throw new IllegalArgumentException("A == B || A == C || A == D || A == E || B == C || B == D || B == E || C == D || C == E || D == E")
  (x, y) => {
    if (ac.isInstance(x) && ac.isInstance(y)) a.unify(x.asInstanceOf[A], y.asInstanceOf[A])
    else if (bc.isInstance(x) && bc.isInstance(y)) b.unify(x.asInstanceOf[B], y.asInstanceOf[B])
    else if (cc.isInstance(x) && cc.isInstance(y)) c.unify(x.asInstanceOf[C], y.asInstanceOf[C])
    else if (dc.isInstance(x) && dc.isInstance(y)) d.unify(x.asInstanceOf[D], y.asInstanceOf[D])
    else if (ec.isInstance(x) && ec.isInstance(y)) e.unify(x.asInstanceOf[E], y.asInstanceOf[E])
    else Unifying.failure
  }
}
@targetName("U$Union_") def U$Union[A, B, C, D, E, F](implicit ar: => Unifier[A], br: => Unifier[B], cr: => Unifier[C], dr: => Unifier[D], er: => Unifier[E], fr: => Unifier[F], aev: ClassTag[A], bev: ClassTag[B], cev: ClassTag[C], dev: ClassTag[D], eev: ClassTag[E], fev: ClassTag[F]): Unifier[A | B | C | D | E | F] = U$Union(ar, br, cr, dr, er, fr)(aev, bev, cev, dev, eev, fev)
def U$Union[A, B, C, D, E, F](ar: => Unifier[A], br: => Unifier[B], cr: => Unifier[C], dr: => Unifier[D], er: => Unifier[E], fr: => Unifier[F])(implicit aev: ClassTag[A], bev: ClassTag[B], cev: ClassTag[C], dev: ClassTag[D], eev: ClassTag[E], fev: ClassTag[F]): Unifier[A | B | C | D | E | F] = {
  lazy val a = ar
  lazy val b = br
  lazy val c = cr
  lazy val d = dr
  lazy val e = er
  lazy val f = fr
  val ac = aev.runtimeClass
  val bc = bev.runtimeClass
  val cc = cev.runtimeClass
  val dc = dev.runtimeClass
  val ec = eev.runtimeClass
  val fc = fev.runtimeClass
  if (ac == bc || ac == cc || ac == dc || ac == ec || ac == fc || bc == cc || bc == dc || bc == ec || bc == fc || cc == dc || cc == ec || cc == fc || dc == ec || dc == fc || ec == fc) throw new IllegalArgumentException("A == B || A == C || A == D || A == E || A == F || B == C || B == D || B == E || B == F || C == D || C == E || C == F || D == E || D == F || E == F")
  (x, y) => {
    if (ac.isInstance(x) && ac.isInstance(y)) a.unify(x.asInstanceOf[A], y.asInstanceOf[A])
    else if (bc.isInstance(x) && bc.isInstance(y)) b.unify(x.asInstanceOf[B], y.asInstanceOf[B])
    else if (cc.isInstance(x) && cc.isInstance(y)) c.unify(x.asInstanceOf[C], y.asInstanceOf[C])
    else if (dc.isInstance(x) && dc.isInstance(y)) d.unify(x.asInstanceOf[D], y.asInstanceOf[D])
    else if (ec.isInstance(x) && ec.isInstance(y)) e.unify(x.asInstanceOf[E], y.asInstanceOf[E])
    else if (fc.isInstance(x) && fc.isInstance(y)) f.unify(x.asInstanceOf[F], y.asInstanceOf[F])
    else Unifying.failure
  }
}
@targetName("U$Union_") def U$Union[A, B, C, D, E, F, G](implicit ar: => Unifier[A], br: => Unifier[B], cr: => Unifier[C], dr: => Unifier[D], er: => Unifier[E], fr: => Unifier[F], gr: => Unifier[G], aev: ClassTag[A], bev: ClassTag[B], cev: ClassTag[C], dev: ClassTag[D], eev: ClassTag[E], fev: ClassTag[F], gev: ClassTag[G]): Unifier[A | B | C | D | E | F | G] = U$Union(ar, br, cr, dr, er, fr, gr)(aev, bev, cev, dev, eev, fev, gev)
def U$Union[A, B, C, D, E, F, G](ar: => Unifier[A], br: => Unifier[B], cr: => Unifier[C], dr: => Unifier[D], er: => Unifier[E], fr: => Unifier[F], gr: => Unifier[G])(implicit aev: ClassTag[A], bev: ClassTag[B], cev: ClassTag[C], dev: ClassTag[D], eev: ClassTag[E], fev: ClassTag[F], gev: ClassTag[G]): Unifier[A | B | C | D | E | F | G] = {
  lazy val a = ar
  lazy val b = br
  lazy val c = cr
  lazy val d = dr
  lazy val e = er
  lazy val f = fr
  lazy val g = gr
  val ac = aev.runtimeClass
  val bc = bev.runtimeClass
  val cc = cev.runtimeClass
  val dc = dev.runtimeClass
  val ec = eev.runtimeClass
  val fc = fev.runtimeClass
  val gc = gev.runtimeClass
  if (ac == bc || ac == cc || ac == dc || ac == ec || ac == fc || ac == gc || bc == cc || bc == dc || bc == ec || bc == fc || bc == gc || cc == dc || cc == ec || cc == fc || cc == gc || dc == ec || dc == fc || dc == gc || ec == fc || ec == gc || fc == gc) throw new IllegalArgumentException("A == B || A == C || A == D || A == E || A == F || A == G || B == C || B == D || B == E || B == F || B == G || C == D || C == E || C == F || C == G || D == E || D == F || D == G || E == F || E == G || F == G")
  (x, y) => {
    if (ac.isInstance(x) && ac.isInstance(y)) a.unify(x.asInstanceOf[A], y.asInstanceOf[A])
    else if (bc.isInstance(x) && bc.isInstance(y)) b.unify(x.asInstanceOf[B], y.asInstanceOf[B])
    else if (cc.isInstance(x) && cc.isInstance(y)) c.unify(x.asInstanceOf[C], y.asInstanceOf[C])
    else if (dc.isInstance(x) && dc.isInstance(y)) d.unify(x.asInstanceOf[D], y.asInstanceOf[D])
    else if (ec.isInstance(x) && ec.isInstance(y)) e.unify(x.asInstanceOf[E], y.asInstanceOf[E])
    else if (fc.isInstance(x) && fc.isInstance(y)) f.unify(x.asInstanceOf[F], y.asInstanceOf[F])
    else if (gc.isInstance(x) && gc.isInstance(y)) g.unify(x.asInstanceOf[G], y.asInstanceOf[G])
    else Unifying.failure
  }
}
@targetName("U$Union_") def U$Union[A, B, C, D, E, F, G, H](implicit ar: => Unifier[A], br: => Unifier[B], cr: => Unifier[C], dr: => Unifier[D], er: => Unifier[E], fr: => Unifier[F], gr: => Unifier[G], hr: => Unifier[H], aev: ClassTag[A], bev: ClassTag[B], cev: ClassTag[C], dev: ClassTag[D], eev: ClassTag[E], fev: ClassTag[F], gev: ClassTag[G], hev: ClassTag[H]): Unifier[A | B | C | D | E | F | G | H] = U$Union(ar, br, cr, dr, er, fr, gr, hr)(aev, bev, cev, dev, eev, fev, gev, hev)
def U$Union[A, B, C, D, E, F, G, H](ar: => Unifier[A], br: => Unifier[B], cr: => Unifier[C], dr: => Unifier[D], er: => Unifier[E], fr: => Unifier[F], gr: => Unifier[G], hr: => Unifier[H])(implicit aev: ClassTag[A], bev: ClassTag[B], cev: ClassTag[C], dev: ClassTag[D], eev: ClassTag[E], fev: ClassTag[F], gev: ClassTag[G], hev: ClassTag[H]): Unifier[A | B | C | D | E | F | G | H] = {
  lazy val a = ar
  lazy val b = br
  lazy val c = cr
  lazy val d = dr
  lazy val e = er
  lazy val f = fr
  lazy val g = gr
  lazy val h = hr
  val ac = aev.runtimeClass
  val bc = bev.runtimeClass
  val cc = cev.runtimeClass
  val dc = dev.runtimeClass
  val ec = eev.runtimeClass
  val fc = fev.runtimeClass
  val gc = gev.runtimeClass
  val hc = hev.runtimeClass
  if (ac == bc || ac == cc || ac == dc || ac == ec || ac == fc || ac == gc || ac == hc || bc == cc || bc == dc || bc == ec || bc == fc || bc == gc || bc == hc || cc == dc || cc == ec || cc == fc || cc == gc || cc == hc || dc == ec || dc == fc || dc == gc || dc == hc || ec == fc || ec == gc || ec == hc || fc == gc || fc == hc || gc == hc) throw new IllegalArgumentException("A == B || A == C || A == D || A == E || A == F || A == G || A == H || B == C || B == D || B == E || B == F || B == G || B == H || C == D || C == E || C == F || C == G || C == H || D == E || D == F || D == G || D == H || E == F || E == G || E == H || F == G || F == H || G == H")
  (x, y) => {
    if (ac.isInstance(x) && ac.isInstance(y)) a.unify(x.asInstanceOf[A], y.asInstanceOf[A])
    else if (bc.isInstance(x) && bc.isInstance(y)) b.unify(x.asInstanceOf[B], y.asInstanceOf[B])
    else if (cc.isInstance(x) && cc.isInstance(y)) c.unify(x.asInstanceOf[C], y.asInstanceOf[C])
    else if (dc.isInstance(x) && dc.isInstance(y)) d.unify(x.asInstanceOf[D], y.asInstanceOf[D])
    else if (ec.isInstance(x) && ec.isInstance(y)) e.unify(x.asInstanceOf[E], y.asInstanceOf[E])
    else if (fc.isInstance(x) && fc.isInstance(y)) f.unify(x.asInstanceOf[F], y.asInstanceOf[F])
    else if (gc.isInstance(x) && gc.isInstance(y)) g.unify(x.asInstanceOf[G], y.asInstanceOf[G])
    else if (hc.isInstance(x) && hc.isInstance(y)) h.unify(x.asInstanceOf[H], y.asInstanceOf[H])
    else Unifying.failure
  }
}
@targetName("U$Union_") def U$Union[A, B, C, D, E, F, G, H, I](implicit ar: => Unifier[A], br: => Unifier[B], cr: => Unifier[C], dr: => Unifier[D], er: => Unifier[E], fr: => Unifier[F], gr: => Unifier[G], hr: => Unifier[H], ir: => Unifier[I], aev: ClassTag[A], bev: ClassTag[B], cev: ClassTag[C], dev: ClassTag[D], eev: ClassTag[E], fev: ClassTag[F], gev: ClassTag[G], hev: ClassTag[H], iev: ClassTag[I]): Unifier[A | B | C | D | E | F | G | H | I] = U$Union(ar, br, cr, dr, er, fr, gr, hr, ir)(aev, bev, cev, dev, eev, fev, gev, hev, iev)
def U$Union[A, B, C, D, E, F, G, H, I](ar: => Unifier[A], br: => Unifier[B], cr: => Unifier[C], dr: => Unifier[D], er: => Unifier[E], fr: => Unifier[F], gr: => Unifier[G], hr: => Unifier[H], ir: => Unifier[I])(implicit aev: ClassTag[A], bev: ClassTag[B], cev: ClassTag[C], dev: ClassTag[D], eev: ClassTag[E], fev: ClassTag[F], gev: ClassTag[G], hev: ClassTag[H], iev: ClassTag[I]): Unifier[A | B | C | D | E | F | G | H | I] = {
  lazy val a = ar
  lazy val b = br
  lazy val c = cr
  lazy val d = dr
  lazy val e = er
  lazy val f = fr
  lazy val g = gr
  lazy val h = hr
  lazy val i = ir
  val ac = aev.runtimeClass
  val bc = bev.runtimeClass
  val cc = cev.runtimeClass
  val dc = dev.runtimeClass
  val ec = eev.runtimeClass
  val fc = fev.runtimeClass
  val gc = gev.runtimeClass
  val hc = hev.runtimeClass
  val ic = iev.runtimeClass
  if (ac == bc || ac == cc || ac == dc || ac == ec || ac == fc || ac == gc || ac == hc || ac == ic || bc == cc || bc == dc || bc == ec || bc == fc || bc == gc || bc == hc || bc == ic || cc == dc || cc == ec || cc == fc || cc == gc || cc == hc || cc == ic || dc == ec || dc == fc || dc == gc || dc == hc || dc == ic || ec == fc || ec == gc || ec == hc || ec == ic || fc == gc || fc == hc || fc == ic || gc == hc || gc == ic || hc == ic) throw new IllegalArgumentException("A == B || A == C || A == D || A == E || A == F || A == G || A == H || A == I || B == C || B == D || B == E || B == F || B == G || B == H || B == I || C == D || C == E || C == F || C == G || C == H || C == I || D == E || D == F || D == G || D == H || D == I || E == F || E == G || E == H || E == I || F == G || F == H || F == I || G == H || G == I || H == I")
  (x, y) => {
    if (ac.isInstance(x) && ac.isInstance(y)) a.unify(x.asInstanceOf[A], y.asInstanceOf[A])
    else if (bc.isInstance(x) && bc.isInstance(y)) b.unify(x.asInstanceOf[B], y.asInstanceOf[B])
    else if (cc.isInstance(x) && cc.isInstance(y)) c.unify(x.asInstanceOf[C], y.asInstanceOf[C])
    else if (dc.isInstance(x) && dc.isInstance(y)) d.unify(x.asInstanceOf[D], y.asInstanceOf[D])
    else if (ec.isInstance(x) && ec.isInstance(y)) e.unify(x.asInstanceOf[E], y.asInstanceOf[E])
    else if (fc.isInstance(x) && fc.isInstance(y)) f.unify(x.asInstanceOf[F], y.asInstanceOf[F])
    else if (gc.isInstance(x) && gc.isInstance(y)) g.unify(x.asInstanceOf[G], y.asInstanceOf[G])
    else if (hc.isInstance(x) && hc.isInstance(y)) h.unify(x.asInstanceOf[H], y.asInstanceOf[H])
    else if (ic.isInstance(x) && ic.isInstance(y)) i.unify(x.asInstanceOf[I], y.asInstanceOf[I])
    else Unifying.failure
  }
}

trait EqualUnifier[T] extends Unifier[T] {
  override def concreteUnify(self: T, other: T): Unifying[Unit] = Unifying.guard(self == other)
}

def equalUnifier[T]: Unifier[T] = new EqualUnifier[T] {}

implicit object U$Symbol extends EqualUnifier[Symbol]

implicit object U$String extends EqualUnifier[String]

implicit object U$Unit extends EqualUnifier[Unit]

implicit object U$Int extends EqualUnifier[Int]

implicit object U$Long extends EqualUnifier[Long]

implicit object U$Float extends EqualUnifier[Float]

implicit object U$Double extends EqualUnifier[Double]

implicit object U$Integer extends EqualUnifier[Integer]

implicit object U$Boolean extends EqualUnifier[Boolean]

def U$Product[T, R <: Product1[T]](implicit tr: => Unifier[T]): Unifier[R] = {
  lazy val t = tr
  (x, y) =>
    if (x.getClass != y.getClass) Unifying.failure else t.unify(x._1, y._1)
}

def U$Product[A, B, R <: Product2[A, B]](implicit ar: => Unifier[A], br: => Unifier[B]): Unifier[R] = {
  lazy val a = ar
  lazy val b = br
  (x, y) =>
    if (x.getClass != y.getClass) Unifying.failure else for {
      _ <- a.unify(x._1, y._1)
      _ <- b.unify(x._2, y._2)
    } yield ()
}

def U$Product[A, B, C, R <: Product3[A, B, C]](implicit ar: Unifier[A], br: Unifier[B], cr: Unifier[C]): Unifier[R] = {
  lazy val a = ar
  lazy val b = br
  lazy val c = cr
  (x, y) =>
    if (x.getClass != y.getClass) Unifying.failure else for {
      _ <- a.unify(x._1, y._1)
      _ <- b.unify(x._2, y._2)
      _ <- c.unify(x._3, y._3)
    } yield ()
}
def U$Product[A, B, C, D, R <: Product4[A, B, C, D]](implicit ar: Unifier[A], br: Unifier[B], cr: Unifier[C], dr: Unifier[D]): Unifier[R] = {
  lazy val a = ar
  lazy val b = br
  lazy val c = cr
  lazy val d = dr
  (x, y) =>
    if (x.getClass != y.getClass) Unifying.failure else for {
      _ <- a.unify(x._1, y._1)
      _ <- b.unify(x._2, y._2)
      _ <- c.unify(x._3, y._3)
      _ <- d.unify(x._4, y._4)
    } yield ()
}
def U$Product[A, B, C, D, E, R <: Product5[A, B, C, D, E]](implicit ar: Unifier[A], br: Unifier[B], cr: Unifier[C], dr: Unifier[D], er: Unifier[E]): Unifier[R] = {
  lazy val a = ar
  lazy val b = br
  lazy val c = cr
  lazy val d = dr
  lazy val e = er
  (x, y) =>
    if (x.getClass != y.getClass) Unifying.failure else for {
      _ <- a.unify(x._1, y._1)
      _ <- b.unify(x._2, y._2)
      _ <- c.unify(x._3, y._3)
      _ <- d.unify(x._4, y._4)
      _ <- e.unify(x._5, y._5)
    } yield ()
}