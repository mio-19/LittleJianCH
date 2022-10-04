package littlejian.data

import littlejian._

type Prod2[T, U] = Product2[VarOr[T],VarOr[U]]
type Tup2[T, U] = (VarOr[T], VarOr[U])
type Prod3[T, U, V] = Product3[VarOr[T],VarOr[U],VarOr[V]]
type Tup3[T, U, V] = (VarOr[T], VarOr[U], VarOr[V])
type Prod4[T, U, V, W] = Product4[VarOr[T],VarOr[U],VarOr[V],VarOr[W]]
type Tup4[T, U, V, W] = (VarOr[T], VarOr[U], VarOr[V], VarOr[W])
type Prod5[T, U, V, W, X] = Product5[VarOr[T],VarOr[U],VarOr[V],VarOr[W],VarOr[X]]
type Tup5[T, U, V, W, X] = (VarOr[T], VarOr[U], VarOr[V], VarOr[W], VarOr[X])