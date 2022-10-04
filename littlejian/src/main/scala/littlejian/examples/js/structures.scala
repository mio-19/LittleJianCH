package littlejian.examples.js

import littlejian._
import littlejian.ext._
import littlejian.data.sexp._

// https://github.com/Artish357/RelateJS/blob/master/js-structures.rkt

/*
(define (jlet key value exp)
  `(let ,key ,value ,exp))

(define (jfun params body)
  `(fun ,params ,body))

(define (jclo params body env)
  `(jclosure ,params ,body ,env))

(define (japp closure args)
  `(app ,closure ,args))

(define (jget obj key)
  `(get ,obj ,key))

(define (jset obj key value)
  `(set ,obj ,key ,value))

(define (jdel obj key)
  `(delete ,obj ,key))

(define (jvar var)
  `(var ,var))

(define (jrawnum n)
  `(number ,n))

(define (jnum n)
  `(number ,(build-num n)))

(define (jobj bindings)
  `(object ,bindings))

(define (jall value)
  `(allocate ,value))

(define (jref value)
  `(ref ,value))

(define (jderef address)
  `(deref ,address))

(define (jassign var val)
  `(assign ,var ,val))

(define (jbeg first second)
  `(begin ,first ,second))

(define (jbool bool)
  `(boolean ,bool))

(define (jif cond then else)
  `(if ,cond ,then ,else))

(define (jundef)
  '(undefined))

(define (jnul)
  '(null))

(define (jwhile cond body)
  `(while ,cond ,body))

(define (jthrow label value)
  `(throw ,label ,value))

(define (jbrk label value)
  `(break ,label ,value))

(define (jfin try-exp fin-exp)
  `(finally ,try-exp ,fin-exp))

(define (jcatch label try-exp catch-var catch-exp)
  `(catch ,label ,try-exp ,catch-var ,catch-exp))

(define (jrawstr str)
  `(string ,str))

(define (jstr str)
  `(string ,(map (compose1 build-num char->integer) (string->list str))))

(define (jdelta fun vals)
  `(delta ,fun ,vals))

(define (jpass) 'pass)
*/
def jlet(key: VarOr[JSData], value: VarOr[JSData], exp: VarOr[JSData]): JSData = list("let", key, value, exp)
def jfun(params: VarOr[SExp], body: VarOr[SExp]): SExp = list("fun", params, body)
def jclo(params: VarOr[SExp], body: VarOr[SExp], env: VarOr[SExp]): SExp = list("jclosure", params, body, env)
def japp(closure: VarOr[SExp], args: VarOr[SExp]): SExp = list("app", closure, args)
def jget(obj: VarOr[SExp], key: VarOr[SExp]): SExp = list("get", obj, key)
def jset(obj: VarOr[SExp], key: VarOr[SExp], value: VarOr[SExp]): SExp = list("set", obj, key, value)
def jdel(obj: VarOr[SExp], key: VarOr[SExp]): SExp = list("delete", obj, key)
def jvar(varName: VarOr[SExp]): SExp = list("var", varName)
def jrawnum(n: VarOr[SExp]): SExp = list("number", n)
// def jnum(n: VarOr[SExp]): SExp = list("number", buildNum(n)) // TODO
def jobj(bindings: VarOr[SExp]): SExp = list("object", bindings)
def jall(value: VarOr[SExp]): SExp = list("allocate", value)
def jref(value: VarOr[SExp]): SExp = list("ref", value)
def jderef(address: VarOr[SExp]): SExp = list("deref", address)
def jassign(varName: VarOr[SExp], value: VarOr[SExp]): SExp = list("assign", varName, value)
def jbeg(first: VarOr[SExp], second: VarOr[SExp]): SExp = list("begin", first, second)
def jbool(bool: VarOr[SExp]): SExp = list("boolean", bool)
def jif(cond: VarOr[SExp], thenExp: VarOr[SExp], elseExp: VarOr[SExp]): SExp = list("if", cond, thenExp, elseExp)
def jundef(): SExp = list("undefined")
def jnul(): SExp = list("null")
def jwhile(cond: VarOr[SExp], body: VarOr[SExp]): SExp = list("while", cond, body)
def jthrow(label: VarOr[SExp], value: VarOr[SExp]): SExp = list("throw", label, value)
def jbrk(label: VarOr[SExp], value: VarOr[SExp]): SExp = list("break", label, value)
def jfin(tryExp: VarOr[SExp], finExp: VarOr[SExp]): SExp = list("finally", tryExp, finExp)
def jcatch(label: VarOr[SExp], tryExp: VarOr[SExp], catchVar: VarOr[SExp], catchExp: VarOr[SExp]): SExp = list("catch", label, tryExp, catchVar, catchExp)
def jrawstr(str: VarOr[SExp]): SExp = list("string", str)
//def jstr(str: VarOr[SExp]): SExp = list("string", map(compose1(buildNum, char2Int), string2List(str))) // TODO
def jdelta(fun: VarOr[SExp], vals: VarOr[SExp]): SExp = list("delta", fun, vals)
def jpass(): SExp = list("pass")