#lang plai-typed

;; it's objects all the way down. Until you hit turtles

(require "python-core-syntax.rkt"
         "python-primitives.rkt"
         "python-objects.rkt"
         "python-interp-helpers.rkt")

(require [typed-in racket (hash->list : ((hashof 'a 'b) -> (listof ('a * 'b))))])
(require [typed-in racket (car : (('a * 'b) -> 'a))])
(require [typed-in racket (cdr : (('a * 'b) -> 'b))])

(define (hash-has-key? (ht : (hashof 'a 'b)) (key : 'a)) : boolean
  (type-case (optionof 'b) (hash-ref ht key)
    [some (n) #t]
    [none () #f]))

(define (search-obj (obj : CVal) (field : string)) : CVal
  (type-case CVal obj
    [VObject (pval fields) (type-case (optionof CVal) (hash-ref fields field) 
                             [some (n) n]
                             [none () (type-case (optionof CVal) (hash-ref fields "%class")
                                        [some (n) (search-obj n field)]
                                        [none () (error 'search-obj (string-append "Sorry, dude. Not here: " field))])])]))

(define (extract-string (val : PrimVal)) : string
  (type-case PrimVal val
    [VStr (s) s]
    [else (error 'extract-string "Expected string. Blowing up")]))

(define none-obj (VObject (VNone) (make-hash empty)))

(define (interp-env (expr : CExp) (env : Env)) : CVal
  (type-case CExp expr
    [CError (e) (error 'interp (to-string (interp-env e env)))]
    
    [CIf (i t e) (if (booleanof (interp-env i env))
                     (interp-env t env)
                     (interp-env e env))]
    
    [CId (x) (type-case (optionof CVal) (hash-ref env x)
               [some (v) v]
               [none () (error 'interp (foldl string-append 
                                              "" 
                                              (append (list "Unbound identifier: " 
                                                            (symbol->string x)
                                                            "\nIn environment:"
                                                            )
                                                      (map (lambda (field) 
                                                             (string-append (symbol->string (car field)) " "))
                                                           (hash->list env)))))])]
    [CLet (x bind body)
          (interp-env body (hash-set env x (interp-env bind env)))]
    [CList (mutable elts) (interp-env (%to-object (VList mutable (map (lambda (x) (interp-env x env)) elts))) env)]
    [CSeq (e1 e2)
          (begin (interp-env e1 env) 
                 (interp-env e2 env))]
    [CPass () none-obj]
    [CApp (fun arges)
          (type-case CVal (interp-env fun env)
            [VObject (primval fields) (type-case PrimVal primval
                                        [VClosure (cenv argxs body)
                                                  (local [(define argvs (map (lambda (e) (interp-env e env)) arges))]
                                                    (interp-env body (bind-args argxs argvs cenv)))]
                                        [else (error 'interp "Not a closure")])])]
    [CFunc (args body) (VObject (VClosure env args body) (make-hash empty))] 
    [CSet! (a b) (error 'CSet! "Not yet implemented")]
    [CPrim1 (prim arg) (python-prim1 prim (interp-env arg env))]
    [CPrim2 (op left right) (interp-env (Combine op (interp-env left env) (interp-env right env)) env)]
    [CObject (type pval fields) (VObject pval (make-hash (map 
                                                          (lambda (field) 
                                                            (let ([key (car field)]
                                                                  [value (cdr field)])
                                                              (values key (interp-env value env))))
                                                          (hash->list fields))))]
    [CSetField (obj field value) (type-case CVal (interp-env obj env) 
                                   [VObject (primval fields) (begin
                                                               (hash-set! fields (obj-to-string (interp-env field env)) (interp-env value env))
                                                               none-obj)])]
    [CGetField (obj field) (search-obj (interp-env obj env) (obj-to-string (interp-env field env)))]))

(define (bind-args args vals env)
  (cond [(and (empty? args) (empty? vals)) env]
        [(or (empty? args) (empty? vals))
         (error 'interp "Arity mismatch")]
        [(and (cons? args) (cons? vals))
         (hash-set (bind-args (rest args) (rest vals) env)
                   (first args) (first vals))]))

(define (interp expr)
  (interp-env expr (hash (list))))