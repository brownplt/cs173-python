#lang plai-typed

(define-type CExp
  [CUndefined]
  [CNone]
  [CTrue]
  [CFalse]
  [CNum (n : number)]
  [CStr (s : string)]
  [CBox (v : CExp)]
  [CObj (dict : CExp) (class : CExp)]
  [CPrimMap (vals : (listof (CExp * CExp)))] ;; an immutable map
  [CTuple (l : (listof CExp))] ;; need a way to make tuples without
                           ;; applying a primf (which needs a tuple)
  [CPrimF (id : symbol)] ;; primitive functions -- applied like lambdas,
                         ;; defined in racket
  [CSeq (e1 : CExp) (e2 : CExp)]
  [CId (id : symbol)]
  [CLet (id : symbol) (bind : CExp) (body : CExp)]
  [CAddGlobal (id : symbol) (bind : CExp)] ;; hack to let the
                                           ;; primitive runtime know
                                           ;; what the values defined
                                           ;; in lib are
  [CSet! (id : symbol) (value : CExp)]
  [CApp (fun : CExp) (args : (listof CExp)) (varargs : CExp)]
  [CFunc (args : (listof symbol)) (vararg : (optionof symbol)) (body : CExp)]
  [CReturn (value : CExp)]
  [CIf (test : CExp) (then : CExp) (else : CExp)]
  [CError (val : CExp)])

(define-type CVal
  [VUndefined];;initial value for variables
  [VNone]
  [VTrue]
  [VFalse]
  [VNum (n : number)]
  [VStr (s : string)]
  [VBox (v : Location)]
  [VObj (dict : CVal) (class : CVal)]
  [VPrimMap (m : (hashof CVal CVal))]
  [VTuple (l : (listof CVal))]
  ;;[VList (l : Location)] ;; implement using __getattr__ + delegation
  ;;[VDict (l : Location)] ;; implement using __getattr__ + delegation
  [VClosure (env : Env) (args : (listof symbol)) (vararg : (optionof symbol)) (body : CExp)]
  [VPrimF (id : symbol)])

(define-type-alias Location number)
(define-type-alias Env (hashof symbol Location))
(define-type-alias Store (hashof Location CVal))

