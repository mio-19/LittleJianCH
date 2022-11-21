package littlejian.examples.js

import littlejian._
import littlejian.ext._
import littlejian.data._
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
def jlet(key: VarOr[JSData], value: VarOr[JSData], exp: VarOr[JSData]): JSData = LList("let", key, value, exp)
def jfun(params: VarOr[JSData], body: VarOr[JSData]): JSData = LList("fun", params, body)
def jclo(params: VarOr[JSData], body: VarOr[JSData], env: VarOr[JSData]): JSData = LList("jclosure", params, body, env)
def japp(closure: VarOr[JSData], args: VarOr[JSData]): JSData = LList("app", closure, args)
def jget(obj: VarOr[JSData], key: VarOr[JSData]): JSData = LList("get", obj, key)
def jset(obj: VarOr[JSData], key: VarOr[JSData], value: VarOr[JSData]): JSData = LList("set", obj, key, value)
def jdel(obj: VarOr[JSData], key: VarOr[JSData]): JSData = LList("delete", obj, key)
def jvar(vari: VarOr[JSData]): JSData = LList("var", vari)
def jrawnum(n: VarOr[JSData]): JSData = LList("number", n)
def jnum(n: Int): JSData = LList("number", Int32.from(n))
def jobj(bindings: VarOr[JSData]): JSData = LList("object", bindings)
def jall(value: VarOr[JSData]): JSData = LList("allocate", value)
def jref(value: VarOr[JSData]): JSData = LList("ref", value)
def jderef(address: VarOr[JSData]): JSData = LList("deref", address)
def jassign(vari: VarOr[JSData], value: VarOr[JSData]): JSData = LList("assign", vari, value)
def jbeg(first: VarOr[JSData], second: VarOr[JSData]): JSData = LList("begin", first, second)
def jbool(bool: VarOr[JSData]): JSData = LList("boolean", bool)
def jif(cond: VarOr[JSData], thenBlock: VarOr[JSData], els: VarOr[JSData]): JSData = LList("if", cond, thenBlock, els)
val jundef: JSData = LList("undefined")
val jnul: JSData = LList("null")
def jwhile(cond: VarOr[JSData], body: VarOr[JSData]): JSData = LList("while", cond, body)
def jthrow(label: VarOr[JSData], value: VarOr[JSData]): JSData = LList("throw", label, value)
def jbrk(label: VarOr[JSData], value: VarOr[JSData]): JSData = LList("break", label, value)
def jfin(tryExp: VarOr[JSData], finExp: VarOr[JSData]): JSData = LList("finally", tryExp, finExp)
def jcatch(label: VarOr[JSData], tryExp: VarOr[JSData], catchVar: VarOr[JSData], catchExp: VarOr[JSData]): JSData = LList("catch", label, tryExp, catchVar, catchExp)
def jrawstr(str: VarOr[JSData]): JSData = LList("string", str)
def jstr(str: String): JSData = LList("string", Str(str))
def jdelta(fun: VarOr[JSData], vals: VarOr[JSData]): JSData = LList("delta", fun, vals)
def jpass(): JSData = LList("pass")
