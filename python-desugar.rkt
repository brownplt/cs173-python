#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt")

(define (desugar expr)
  (type-case PyExpr expr
    [PyNum (n) (CNum n)]
    [PyApp (f args) (CApp (desugar f) (map desugar args))]
    [PyId (x) (CId x)]))
