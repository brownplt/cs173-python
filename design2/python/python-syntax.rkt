#lang plai-typed

(define-type PyExpr
  [PyGlobal (names : (listof symbol))]
  [PyNonlocal (names : (listof symbol))]            
  [PySeq (es : (listof PyExpr))]
  [PyAssign (value : PyExpr) (targets : (listof symbol))]
  [PyReal (n : number)]
  [PyPass]
  [PyStr (s : string)]
  [PyComplex (n : number)]
  [PyFunc (id : symbol) (args : (listof symbol)) (body : PyExpr)]
  [PyReturn (value : PyExpr)]
  ;[PyTryFinally (body : PyExpr) (finally : PyExpr)] ; Always runs 'finally' on exit of body
  ;[PyTryCatch (body : PyExpr) (handlers : (listof) (orelse : PyExpr)] ; 
  [PyId (x : symbol)]
  [PyIf (tst : PyExpr) (thn : PyExpr) (els : PyExpr)]
  [PyApp (fun : PyExpr) (args : (listof PyExpr))])


