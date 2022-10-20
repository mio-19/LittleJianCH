package littlejian.ext

import littlejian._

import scala.language.implicitConversions

implicit class GoalWithAppReversed1[A, R](a: GoalWith[A]) {
  @inline def app(fn: A => GoalWith[R]): GoalWith[R] = for {
    x <- a
    r <- fn(x)
  } yield r
}

implicit class GoalWithApp1[A, R](fn: A => GoalWith[R]) {
  @inline def app(a: GoalWith[A]): GoalWith[R] = for {
    x <- a
    r <- fn(x)
  } yield r
}

implicit class GoalWithApp2[A, B, R](fn: (A, B) => GoalWith[R]) {
  @inline def app(a: GoalWith[A], b: GoalWith[B]): GoalWith[R] = for {
    x <- a
    y <- b
    r <- fn(x, y)
  } yield r
}

implicit class GoalWithApp3[A, B, C, R](fn: (A, B, C) => GoalWith[R]) {
  @inline def app(a: GoalWith[A], b: GoalWith[B], c: GoalWith[C]): GoalWith[R] = for {
    x <- a
    y <- b
    z <- c
    r <- fn(x, y, z)
  } yield r
}

implicit class GoalWithApp4[A, B, C, D, R](fn: (A, B, C, D) => GoalWith[R]) {
  @inline def app(a: GoalWith[A], b: GoalWith[B], c: GoalWith[C], d: GoalWith[D]): GoalWith[R] = for {
    x <- a
    y <- b
    z <- c
    w <- d
    r <- fn(x, y, z, w)
  } yield r
}

inline implicit def goalWithApp1[A, R](fn: A => GoalWith[R]): GoalWith[A] => GoalWith[R] = fn.app
inline implicit def goalWithApp2[A, B, R](fn: (A, B) => GoalWith[R]): (GoalWith[A], GoalWith[B]) => GoalWith[R] = fn.app
inline implicit def goalWithApp3[A, B, C, R](fn: (A, B, C) => GoalWith[R]): (GoalWith[A], GoalWith[B], GoalWith[C]) => GoalWith[R] = fn.app
inline implicit def goalWithApp4[A, B, C, D, R](fn: (A, B, C, D) => GoalWith[R]): (GoalWith[A], GoalWith[B], GoalWith[C], GoalWith[D]) => GoalWith[R] = fn.app

implicit class RelFnApp1[A, R](fn: A => R) {
  inline def call(a: GoalWith[A]): GoalWith[R] = for {
    x <- a
  } yield fn(x)
}
implicit class RelFnApp2[A, B, R](fn: (A, B) => R) {
  inline def call(a: GoalWith[A], b: GoalWith[B]): GoalWith[R] = for {
    x <- a
    y <- b
  } yield fn(x, y)
}
implicit class RelFnApp3[A, B, C, R](fn: (A, B, C) => R) {
  inline def call(a: GoalWith[A], b: GoalWith[B], c: GoalWith[C]): GoalWith[R] = for {
    x <- a
    y <- b
    z <- c
  } yield fn(x, y, z)
}
implicit class RelFnApp4[A, B, C, D, R](fn: (A, B, C, D) => R) {
  inline def call(a: GoalWith[A], b: GoalWith[B], c: GoalWith[C], d: GoalWith[D]): GoalWith[R] = for {
    x <- a
    y <- b
    z <- c
    w <- d
  } yield fn(x, y, z, w)
}