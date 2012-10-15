#lang plai-typed

#|

Since there may end up being a large number of primitives that you
implement for python, here is a suggested factoring into a separate
file.  You can add new primitives here by adding new symbols to the
dispatch.  You might also choose to add more than single-arity
primitives here.

|#

(require (typed-in racket/base [display (string -> void)]))

(define (pretty arg)
  (type-case CVal arg
    [VNum (n) (string->number n)]))

(define (print arg)
  (display (pretty arg)))

(define (python-prim op arg)
  (case arg
    [print (begin (print arg) arg)]))

