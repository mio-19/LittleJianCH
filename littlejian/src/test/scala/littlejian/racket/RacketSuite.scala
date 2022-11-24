package littlejian.racket

import littlejian.data.sexp.SExp.parse

class RacketSuite extends munit.FunSuite {
  test("basics") {
    for((code, result) <- Vector(("((lambda (x) x) 'a)", "a"))) {
      assertEquals(eval(parse(code)), parse(result))
    }
  }
}
