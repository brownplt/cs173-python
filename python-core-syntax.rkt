#lang plai-typed

#|

This is the core language; it is just borrowing a few things from 
ParselTongue.

|#

(define-type CExp
  [CNum (n : number)]
  [CSeq (e1 : CExp) (e2 : CExp)]
  [CId (x : symbol)]
  [CLet (x : symbol) (bind : CExp) (body : CExp)]
  [CApp (fun : CExp) (args : (listof CExp))]
  [CFunc (args : (listof symbol)) (body : CExp)]
  [CPrim1 (prim : symbol) (arg : CExp)])

(define-type CVal
  [VNum (n : number)]
  [VClosure (env : Env) (args : (listof symbol)) (body : CExp)])

(define-type-alias Env (hashof symbol CVal))

