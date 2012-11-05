#lang plai-typed

;; this is an even smaller language - this is what we will CPS.
;; note we only have one argument functions - so currying is the order of the day.

(define-type UExp
  ; primitives
  [UNum (n : number)]
  [UStr (s : string)]
  [UBool (b : boolean)]
  [UMap (fs : (hashof UExp UExp))]
  [UList (es : (listof UExp))]
  [UNone]
  ; control
  [ULetCC (k : symbol) (body : UExp)]
  [USeq (e1 : UExp) (e2 : UExp)]
  [UError (e1 : UExp)]
  [UIf (test : UExp) (then : UExp) (else : UExp)]
  ; binding/mutation
  [UId (x : symbol)]
  [ULet (x : symbol) (v : UExp) (body : UExp)]
  [USet (x : symbol) (v : UExp)]
  ; functions / builtins
  [UApp (fun : UExp) (arg : UExp)]
  [UFn (arg : symbol) (body : UExp)]
  ; we still need a couple primitives, for type reflection primarily
  [UPrim (op : symbol) (args : (listof UExp))]
  ; this is a placeholder to allow us to work with interpreters
  ; for the incomplete language. if we try to evaluate a
  ; UUndefined, it will be a racket error, but if we don't,
  ; all will be well.
  [UUndefined])

; here are the types of the values we produce
(define-type UVal
  [VNum (n : number)]
  [VStr (s : string)]
  [VBool (b : boolean)]
  [VMap (fs : (hashof UVal UVal))]
  [VNone]
  [VList (es : (listof UVal))]
  [VClosure (env : Env) (arg : symbol) (body : UExp)]
  [VError (v : UVal)])

(define-type-alias Location number)

;; used for store passing
;; note that we also need to thread the Env because of how you can set stuff.
(define-type VStateT
  [VState (v : UVal) (s : Store) (e : Env)])

(define-type-alias Env (hashof symbol Location))

(define-type-alias Store (hashof Location UVal))