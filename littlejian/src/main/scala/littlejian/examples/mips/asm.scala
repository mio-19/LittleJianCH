package littlejian.examples.mips

import littlejian._
import littlejian.ext._
import littlejian.data._

inline def isRegister(x: VarOr[FixedNat]): Goal = x.assumeLen(5)
inline def isShift(x: VarOr[FixedNat]): Goal = x.assumeLen(5)
inline def is16(x: VarOr[FixedNat]): Goal = x.assumeLen(16)
inline def is26(x: VarOr[FixedNat]): Goal = x.assumeLen(26)
inline def is6(x: VarOr[FixedNat]): Goal = x.assumeLen(6)

val zero6 = FixedNat.from(6, 0)
def registerEncoding(s: VarOr[FixedNat], t: VarOr[FixedNat], d: VarOr[FixedNat], a: VarOr[FixedNat], f: VarOr[FixedNat]): Rel[Int32] =
  (isRegister(s) && isRegister(t) && isRegister(d) && isShift(a) && is6(f)) >>
    zero6
      .append(s)
      .app(_.append(t))
      .app(_.append(d))
      .app(_.append(a))
      .app(_.append(f))
      .app(_.to32)