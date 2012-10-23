#lang plai-typed

(require "python-core-syntax.rkt"
         "python-primitives.rkt")

(define (interp-env expr env)
  (type-case CExp expr
    [CNum (n) (VNum n)]
    [CStr (s) (VStr s)]
    [CTrue () (VTrue)]

    [CError (e) (error 'interp (to-string (interp-env e env)))]

    [CIf (i t e) (type-case CVal (interp-env i env)
      [VTrue () (interp-env t env)]
      [else (interp-env e env)])]

    [CId (x) (type-case (optionof CVal) (hash-ref env x)
      [some (v) v]
      [none () (error 'interp "Unbound identifier")])]

    [CLet (x bind body)
      (interp-env body (hash-set env x (interp-env bind env)))]

    [CSeq (e1 e2)
      (begin (interp-env e1 env) (interp-env e2 env))]

    [CApp (fun arges)
     (type-case CVal (interp-env fun env)
       [VClosure (cenv argxs body)
         (local [(define argvs (map (lambda (e) (interp-env e cenv)) arges))]
          (interp-env body (bind-args argxs argvs env)))]
       [else (error 'interp "Not a closure")])]

    [CFunc (args body) (VClosure env args body)] 

    [CPrim1 (prim arg) (python-prim1 prim (interp-env arg env))]))

(define (bind-args args vals env)
  (cond [(and (empty? args) (empty? vals)) env]
        [(or (empty? args) (empty? vals))
         (error 'interp "Arity mismatch")]
        [(and (cons? args) (cons? vals))
         (hash-set (bind-args (rest args) (rest vals) env)
                   (first args) (first vals))]))

(define (interp expr)
  (interp-env expr (hash (list))))

