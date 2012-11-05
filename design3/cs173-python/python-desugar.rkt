#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt")

(print-only-errors true)

(define (uniq (l : (listof 'a))) : (listof 'a)
  (hash-keys
   (foldl (lambda (x h) (hash-set h x #t))
          (hash empty)
          l)))
(test (uniq empty) empty)
(test (uniq (list 1 2 3 4 3 5 2 3))
      (list 1 2 3 4 5))

(define (find-locals exp)
  (type-case PyExp exp
    [PySet! (id value) (list id)]
    [PySeq (es) (foldl (lambda (exp res)
                         (append (find-locals exp)
                                 res))
                       empty
                       es)]
    [PyIf (test t e)
          (append (find-locals t)
                  (find-locals e))]
    [else empty]))

(define (desugar-inner exp)
  (type-case PyExp exp
    [PyNum (n) (CNum n)]
    [PyTuple (l) (CTuple (map desugar-inner l))]
    [PySeq (es) (foldl (lambda (e1 e2)
                         (CSeq e2 (desugar-inner e1)))
                       (desugar-inner (first es))
                       (rest es))]
    [PyId (x) (CId x)]
    [PySet! (id value) (CSet! id (desugar-inner value))]
    [PyApp (f args varargs) (CApp (desugar-inner f)
                                  (map desugar-inner args)
                                  (desugar-inner varargs))]
    [PyFunc (args vararg body) (CFunc args vararg (desugar-body body))]
    [PyReturn (value) (CReturn (desugar-inner value))]
    [PyIf (test t e)
          (CLet 'test-value (desugar-inner test)
                (CIf (get-and-call (PyId 'test-value)
                                   "__bool__"
                                   empty
                                   (PyTuple empty))
                     (desugar-inner t)
                     (desugar-inner e)))]
    [PyOp (id args)
          (case id
            [(Add) (binop "__add__" (first args) (second args))]
            [(Sub) (binop "__sub__" (first args) (second args))]
            [(USub) (unop "__neg__" (first args))]
            [else (CApp (CPrimF id)
                        (map desugar-inner args)
                        (CTuple empty))])]
    [PyPass () (CNone)]
    ;;[else (error 'desugar (string-append "not implemented: "
    ;;                                     (to-string exp)))]
    ))

(define (get-and-call inner name args vararg)
  (CApp (CApp (CPrimF 'class-lookup)
              (list (desugar-inner inner)
                    (CStr name))
              (CTuple empty))
        (map desugar-inner args)
        (desugar-inner vararg)))

(define (binop name left right)
  (get-and-call left name
                (list right)
                (PyTuple empty)))

(define (unop name left)
  (CApp (CApp (CPrimF 'class-lookup)
              (list (desugar-inner left)
                    (CStr name))
              (CTuple empty))
        empty
        (CTuple empty)))

(define (desugar-body exp)
  (foldl (lambda (id e)
           (CLet id (CUndefined)
                 e))
         (desugar-inner exp)
         (uniq (find-locals exp))))

(define desugar desugar-body)
