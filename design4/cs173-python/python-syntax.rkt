#lang plai-typed

(print-only-errors true)

(define-type PyExp
  [PySeq (es : (listof PyExp))]
  [PyNum (n : number)]
  [PyStr (s : string)]
  [PyBool (b : boolean)]
  [PyNone]
  [PyDict (fs : (hashof PyExp PyExp))]
  [PyId (x : symbol)]
  [PyFunDef (name : symbol) (args : PyArgT) (body : PyExp)]
  [PyLambda (args : PyArgT) (body : PyExp)]
  [PyApp (fun : PyExp) (args : (listof PyExp))]
  [PyReturn (v : PyExp)]
  ;; targets are PyIds
  [PyAssign (targets : (listof PyExp)) (value : PyExp)]
  [PyIf (test : PyExp) (body : PyExp) (orelse : PyExp)]

  [PyBoolOp (op : symbol) (arg1 : PyExp) (arg2 : PyExp)]
  [PyBinOp (op : symbol) (left : PyExp) (right : PyExp)]
  [PyUnaryOp (op : symbol) (arg : PyExp)]
  [PyCompare (ops : (listof symbol)) (cmps : (listof PyExp)) (lft : PyExp)]
  [PyPass]

  [PyWhile (tst : PyExp) (body : PyExp)]
  [PyBreak]
  [PyContinue]

  ; a placeholder that will go away once all of python exists as
  ; PyExprs. This way we can parse everything, and only error if
  ; we actually try to execute something we haven't defined yet.
  [PyUndefined])


(define-type PyArgT
  ;; to add later - varargs, kwargs, etc
  [PyArgs (args : (listof symbol)) (defaults : (listof PyExp))])