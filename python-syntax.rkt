#lang plai-typed

(define-type PyExpr
  [PyNum (n : number)]
  [PyId (x : symbol)]
  [PyApp (fun : PyExpr) (args : (listof PyExpr))])

