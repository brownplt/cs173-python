#lang plai-typed

(print-only-errors true)

(require "python-micro-syntax.rkt")

(require (typed-in racket/base [display : (string -> void)]))

(require (typed-in racket/base [string<? : (string string -> boolean)]))
(require (typed-in racket/base [string<=? : (string string -> boolean)]))
(require (typed-in racket/base [string>? : (string string -> boolean)]))
(require (typed-in racket/base [string>=? : (string string -> boolean)]))

;; unique returns a unique string every time
;; this is so that you can't end up comparing the type of
;; things that should not be considered (ie, control structures, etc)
(define unique
  (let [(n 0)]
    (lambda ()
      (begin
        (set! n (+ n 1))
        (string-append "uniq-" (to-string n))))))

;; typeof returns a VStr representing the type of the value
(define (typeof [arg : UVal]) : UVal
  (type-case UVal arg
    [VNum (_) (VStr "num")]
    [VStr (_) (VStr "str")]
    [VBool (_) (VStr "bool")]
    [VMap (_) (VStr "map")]
    [VList (_) (VStr "list")]
    [VNone () (VStr "none")]
    [else (VStr (unique))]))

(define (pretty arg)
  (type-case UVal arg
    [VNum (n) (to-string n)]
    [VStr (s) s]
    [VBool (b) (if b "true" "false")]
    [else (error 'prim "Can't print this yet")]))

(define (print arg)
  (display (pretty arg)))

;; truthy returns whether a value is a "true" value according
;; to pythons rules
(define (truthy [a : UVal]) : boolean
    (type-case UVal a
        [VBool (b) b]
        [VNum (n) (not (= n 0))]
        [VStr (s) (not (string=? s ""))]
        [VMap (_) #t]
        [VNone () #f]
        [else (error 'undefined (string-append "Don't know about truthiness for val: "
                                               (to-string a)))]))

(test (truthy (VNum 1)) true)
(test (truthy (VNum -1)) true)
(test (truthy (VNum 0)) false)
(test (truthy (VStr "hello")) true)
(test (truthy (VStr "")) false)
(test (truthy (VStr "hello")) true)
(test (truthy (VMap (hash empty))) true)
(test (truthy (VBool true)) true)
(test (truthy (VBool false)) false)
(test (truthy (VNone)) false)


;; prim has primitives that cannot be replicated in the language
;; note that anything that can be replicated is not included here;
;; that means any type checking, error handling, etc. if prim is handed
;; invalid input, it will just blow up.
(define (prim [op : symbol] [args : (listof UVal)]) : UVal
  (case op
    ['print (begin (print (first args)) (first args))]
    ['pretty (VStr (pretty (first args)))]
    ['truthy (VBool (truthy (first args)))]
    ;; using racket equality? Is that okay?
    ['equal (VBool (equal? (first args) (second args)))]
    ['ty (typeof (first args))]
    ['err-v (VError-v (first args))]
    ;; note: prims are not doing type checking. that can be handled in
    ;; language (using 'ty). also, overloading is not handled. for
    ;; funny stuff like "str" * 2, desugar into nested string appends, etc.
    ['* (VNum (* (VNum-n (first args)) (VNum-n (second args))))]
    ['+ (VNum (+ (VNum-n (first args)) (VNum-n (second args))))]
    ['str+ (VStr (string-append (VStr-s (first args)) (VStr-s (second args))))]
    ['- (VNum (- (VNum-n (first args)) (VNum-n (second args))))]
    ['/ (VNum (/ (VNum-n (first args)) (VNum-n (second args))))]
    ['< (VBool (< (VNum-n (first args)) (VNum-n (second args))))]
    ['> (VBool (> (VNum-n (first args)) (VNum-n (second args))))]
    ['<= (VBool (<= (VNum-n (first args)) (VNum-n (second args))))]
    ['>= (VBool (>= (VNum-n (first args)) (VNum-n (second args))))]
    ['str< (VBool (string<? (VStr-s (first args)) (VStr-s (second args))))]
    ['str> (VBool (string>? (VStr-s (first args)) (VStr-s (second args))))]
    ['str<= (VBool (string<=? (VStr-s (first args)) (VStr-s (second args))))]
    ['str>= (VBool (string>=? (VStr-s (first args)) (VStr-s (second args))))]
    ))

