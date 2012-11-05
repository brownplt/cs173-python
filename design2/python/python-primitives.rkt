#lang plai-typed

(require "python-core-syntax.rkt")

#|

Since there may end up being a large number of primitives that you
implement for python, here is a suggested factoring into a separate
file.  You can add new primitives here by adding new symbols to the
dispatch.  You might also choose to add more than single-arity
primitives here.

|#

(require (typed-in racket/base [display : (string -> void)]))

(define (print [arg : CObject]) : void
  (display (string-append (object-str arg) "\n")))

(define (python-prim1 [op : symbol]
                      [arg : CAnswer]) : CAnswer
  (type-case CAnswer arg
    [AObject (v s)
             (case op
               [(print) (begin 
                          (print v)
                          arg)])]
    [AException (v s) (AException v s)]
    [else (error 'python-prim1 "unexpected result")]))

;(define (python-prim2 [op : symbol]
;                      [arg1 : CVal]
;                      [arg2 : CVal]) : CVal
;  (case op
;    [(Eq) ( arg)]))
