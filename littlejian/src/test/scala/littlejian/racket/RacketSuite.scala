package littlejian.racket

import littlejian.data.sexp.SExp.parse

class RacketSuite extends munit.FunSuite {
  test("basics") {
    for ((code, result) <- Vector(
      ("((lambda (x) x) 'a)", "a"),
      ("((lambda (x) (x 'a)) (lambda (x) x))", "a"),
      ("(append (list 'a 'b) (list 'c))", "(a b c)"),
      ("(append (list 'a 'b) 'c)", "(a b . c)"),
      ("(let ([a list]) (a 'a))", "(a)"),
    )) {
      assertEquals(eval(parse(code)), parse(result))
    }
  }
}
