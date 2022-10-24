# LittleJianCH - 小简Kanren


This miniKanren implementation contains a typed dsl in scala3.

`VarOr[T]` is required to use in data structures defined for relational programming.
It is defined as `Var[T] | T`, where `Var[T]` is a class representing a miniKanren variable.

The `Unify` type class needed to be implemented for `a == b` and `a =/= b` usages.
The `Inspect` type class needed to be implemented for `a.absent(b)` usages.
Both type classes support `derive`.

Several search strategies are implemented in the `littlejian.search` packages.

Examples for data stuctures can be found in [littlejian/src/main/scala/littlejian/data](littlejian/src/main/scala/littlejian/data).
Program examples can be found in [littlejian/src/main/scala/littlejian/examples](littlejian/src/main/scala/littlejian/examples), some of which are incompleted.


![image](https://user-images.githubusercontent.com/57285379/192443676-a067a8ee-21b1-43d4-8bf1-c580cf838ae7.png)
