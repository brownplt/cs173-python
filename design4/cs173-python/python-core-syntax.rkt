#lang plai-typed

(print-only-errors true)

#|

This is the high-level core language. The low level is in python-micro-core, and will be CPSd

|#

(define-type CExp
  ; primitives
  [CNum (n : number)]
  [CStr (s : string)]
  [CBool (b : boolean)]
  [CDict (fs : (hashof CExp CExp))]
  [CObj (fs : (listof CField))]
  [CList (es : (listof CExp))]
  [CNone]
  ; control
  ;; mat is a CFn that returns true for an error it will catch
  [CTry (bdy : CExp) (mat : CExp) (cat : CExp)]
  [CRaise (e : CExp)]
  [CSeq (e1 : CExp) (e2 : CExp)]
  [CIf (test : CExp) (then : CExp) (else : CExp)]
  [CPass]
  [CWhile (cnd : CExp) (bdy : CExp)]
  [CBreak]
  [CContinue]
  ; binding/mutation
  [CId (x : symbol)]
  ;; Note: CLet is used in desugaring, but does not come directly from
  ;; any python code - all python assigns turn into CSet's
  [CLet (x : symbol) (bind : CExp) (body : CExp)]
  [CSet (x : symbol) (v : CExp)]
  ; functions / builtins
  [CApp (fun : CExp) (args : (listof CExp))]
  [CFn (args : (listof symbol)) (body : CExp)]
  [CRet (v : CExp)]
  ;; primitives that are needed - primarily for type reflection / value extraction,
  ;; and some primitive operations on numbers, strings
  [CPrim (prim : symbol) (args : (listof CExp))]
  ; this is a placeholder to allow us to work with interpreters
  ; for the incomplete language. if we try to evaluate a
  ; CUndefined, it will be a racket error, but if we don't,
  ; all will be well.
  [CUndefined])

(define-type CField
  [fieldC (n : symbol) (v : CExp)])
