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

(define (pretty (arg  : PrimVal)) : string
  (type-case PrimVal arg
    [VNum (n) (to-string n)]
    [VStr (s) s]
    [VTrue () "True"]
    [VFalse () "False"]
    [VNone () ""]
    [VList (mutable elts) (foldl string-append
                                 ""
                                 (list (if mutable "[" "(") 
                                       (foldl string-append 
                                              "" 
                                              (map (lambda (x) 
                                                     (string-append (obj-to-string x) 
                                                                    " ")) 
                                                   elts))
                                       (if mutable "]" ")")))]
    [VClosure (env args body) (error 'prim "Can't print closures yet")]))

(define (obj-to-string (obj : CVal)) : string
  (type-case CVal obj
    [VObject (primval fields) (pretty primval)]))

(define (print (arg : CVal))
  (display (string-append (obj-to-string arg) "\n")))

(define (python-prim1 (op : symbol) (arg : CVal))
  (case op
    [(print) (begin (print arg) arg)]))

(define (%add left right)
  (+ left right))

