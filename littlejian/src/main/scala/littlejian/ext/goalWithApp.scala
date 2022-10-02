package littlejian.ext

implicit class GoalWithApp1[A, R](fn: A => GoalWith[R]) {
  def app(a: GoalWith[A]): GoalWith[R] = for {
    x <- a
    r <- fn(x)
  } yield r
}

implicit class GoalWithApp2[A, B, R](fn: (A, B) => GoalWith[R]) {
  def app(a: GoalWith[A], b: GoalWith[B]): GoalWith[R] = for {
    x <- a
    y <- b
    r <- fn(x, y)
  } yield r
}

implicit class GoalWithApp3[A, B, C, R](fn: (A, B, C) => GoalWith[R]) {
  def app(a: GoalWith[A], b: GoalWith[B], c: GoalWith[C]): GoalWith[R] = for {
    x <- a
    y <- b
    z <- c
    r <- fn(x, y, z)
  } yield r
}

implicit class GoalWithApp4[A, B, C, D, R](fn: (A, B, C, D) => GoalWith[R]) {
  def app(a: GoalWith[A], b: GoalWith[B], c: GoalWith[C], d: GoalWith[D]): GoalWith[R] = for {
    x <- a
    y <- b
    z <- c
    w <- d
    r <- fn(x, y, z, w)
  } yield r
}