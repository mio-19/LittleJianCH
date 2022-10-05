package littlejian.data

import littlejian._

enum Nat derives Unifier:
  case Zero
  case Succ(prev: VarOr[Nat])

object Nat {
  def from(n: Int): Nat =
    if(n<0) throw new IllegalArgumentException("n must be non-negative")
    else if(n==0) Zero
    else Succ(from(n-1))
}