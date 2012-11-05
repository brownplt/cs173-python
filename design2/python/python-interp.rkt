#lang plai-typed

(require "python-core-syntax.rkt"
         "python-primitives.rkt"
         "python-lib.rkt")

(require (typed-in racket (number->string : (number -> string))))
(require (typed-in "basic.rkt" (pretty-struct : ('_a -> void))))
(require (typed-in "basic.rkt" (pretty-store : (Store -> void))))
(require (typed-in "basic.rkt" (pretty-scopedb : ((hashof symbol Var) -> void))))

;; Returns a new lambda closed by a global location counter. Each call
;; returns a unique location.
(define new-loc
  (let ([n (box 0)])
    (lambda () : Location
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))

;; Find global scope object
(define (global-scope [env : Scope]) : Scope
  (type-case Scope env
    [Slocal (db parent) (global-scope parent)]
    [Sglobal (db) env]))

;; Find "nonlocal" variable starting *with* the provided env. Does not consider globals.
(define (find-nonlocal [id : symbol]
                       [env : Scope]) : (optionof Scope)
  (type-case Scope env
    [Slocal (db parent)
            (type-case (optionof Var) (hash-ref db id)
              [some (v) (type-case Var v
                          [Vlocal (loc isp) (some env)]
                          [Vnonlocal (ptr) (some ptr)]
                          [else (find-nonlocal id parent)])]
              [none () (find-nonlocal id parent)])]
    [Sglobal (db) (none)]))

;; Finds (or makes) a location in the store to set. Will follow nonlocal/global pointers,
;; but will NOT recur up scope tree otherwise. If a location is "Null", it will
;; automatically assign a new location. If variable does not exist in the provided or pointered
;; scope, it will be created. This is pythonic assignment.
(define (find-loc [id : symbol]
                  [env : Scope]) : Location
  (local ([define (nullloc-wrapper db id loc)  ; called when id does exist
            (if (eq? loc NullLoc)
                (let ((loc (new-loc)))
                  (begin 
                    (hash-set! db id (Vlocal loc false))
                    loc))
                loc)]
          [define (new-var db id) ; called when id does not currently exist
            (let ((loc (new-loc)))
              (begin
                (hash-set! db id (Vlocal loc false))
                loc))])
    (local ([define (find-loc/norecur id env)
              (type-case Scope env
                [Slocal (db parent)
                        (type-case (optionof Var) (hash-ref db id)
                          [some (v)
                                (type-case Var v
                                  [Vlocal (loc isp) (nullloc-wrapper db id loc)]
                                  [Vnonlocal (ptr) (error 'find-loc "pointer led to a Vnonlocal?")]
                                  [Vglobal (ptr) (error 'find-loc "pointer led to a Vglobal?")])]
                          [none ()
                                (new-var db id)])]
                [Sglobal (db) 
                         (type-case (optionof Var) (hash-ref db id)
                           [some (var)
                                 (type-case Var var
                                   [Vlocal (loc isp) (nullloc-wrapper db id loc)]
                                   [Vnonlocal (ptr) (error 'find-loc "nonlocal var ptr in global db")]
                                   [Vglobal (ptr) (error 'find-loc "global var ptr in global db")])]
                           [none ()
                                 (new-var db id)])])])
      (type-case Scope env
        [Slocal (db parent)
                (type-case (optionof Var) (hash-ref db id)
                  [some (v)
                        (type-case Var v
                          [Vlocal (loc isp) (nullloc-wrapper db id loc)]
                          [Vnonlocal (ptr) (find-loc/norecur id ptr)]
                          [Vglobal (ptr) (find-loc/norecur id ptr)])]
                  [none ()
                        (new-var db id)])]
        [Sglobal (db) 
                 (find-loc/norecur id env)]))))
  
;; Look-up helper -- looks up and expects the variable to be a Vlocal of the provided
;;                   scope. this is a helper used by lookup.
(define (lookup-norecur [id : symbol]
                        [env : Scope]
                        [store : Store]) : CAnswer
  (let ([db (type-case Scope env 
              [Slocal (db parent) db]
              [Sglobal (db) db])])
    (type-case (optionof Var) (hash-ref db id)
      [some (v) (type-case Var v
                  [Vlocal (loc isp) (ALocation loc store)]
                  [else (error 'look-norecur "referential type found but can't recur")])]
      [none () (AException (make-NameError (list (make-str (string-append "Unbound identifier: "
                                                                                (symbol->string id)))))
                                 store)])))

;; Looks up starting in the local scope, and walks backwards if a var is not
;; defined in the local scope. This is used in variable retrieval operations
;; and implements python's notion of "closing over" variables.
(define (lookup [id : symbol] [env : Scope] [store : Store]) : CAnswer
  (type-case Scope env
    [Slocal (db parent)
            (type-case (optionof Var) (hash-ref db id)
              [some (var)
                    (type-case Var var
                      [Vlocal (loc isp) (ALocation loc store)]
                      [Vnonlocal (ptr) (lookup-norecur id ptr store)]
                      [Vglobal (ptr) (lookup-norecur id ptr store)])]
              [none ()
                    (lookup id parent store)])]
    [Sglobal (db)
             (type-case (optionof Var) (hash-ref db id)
               [some (var) 
                     (type-case Var var
                       [Vlocal (loc isp) (ALocation loc store)]
                       [Vnonlocal (ptr) (error 'lookup "global scope can't have nonlocal ptr")]
                       [Vglobal (ptr) (error 'lookup "global scope can't have global ptr")])]
               [none ()
                     (AException (make-NameError (list (make-str (string-append "Unbound identifier: "
                                                                                (symbol->string id)))))
                                 store)])]))

;; Fetches a value from the store
;; If we cannot find the location, this means an assignment has not yet
;; occured. This is an UnboundLocalError.
(define (fetch [loc : Location] [store : Store]) : CAnswer
  (type-case (optionof CObject) (hash-ref store loc)
    [some (v) (AObject v store)]
    [none () (AException (make-UnboundLocalError (list (make-str (string-append (number->string loc) ": reference before assignment")))) store)]))

;; db-mutable-copy takes an immutable (hashof symbol Var) and creates a new,
;; mutable copy. This is used when we take the immutable closure environment
;; and create an Slocal scope object out of it for the unique application.
(define (db-mutable-copy [input : (hashof symbol Var)]) : (hashof symbol Var)
  (let ([output (make-hash (list))])
    (local ([define (db-mutable-copy/recur (keys : (listof symbol)))
             (cond
               [(empty? keys) output]
               [else (begin
                       (hash-set! output (first keys) (some-v (hash-ref input (first keys))))
                       (db-mutable-copy/recur (rest keys)))])])
      (db-mutable-copy/recur (hash-keys input)))))

;; make-scope-db takes a (hashof symbol VarType) and creates a new,
;; mutable (hashof symbol Var).
(define (make-scope-db [input : FuncVars]
                       [parentenv : Scope]) : (hashof symbol Var)
  (let ([output (make-hash (list))])
    (local ([define (make-scope-db/recur (keys : (listof symbol)))
             (cond
               [(empty? keys) output]
               [else (begin
                       (hash-set! output 
                                  (first keys)
                                  (type-case VarType (some-v (hash-ref input (first keys)))
                                    [VTlocal (isparam) (Vlocal NullLoc isparam)]
                                    [VTnonlocal () (type-case (optionof Scope) (find-nonlocal (first keys) parentenv)
                                                     [some (v) (Vnonlocal v)]
                                                     [none () (error 'make-scope-db
                                                                     (string-append 
                                                                      "SyntaxError: no binding for nonlocal "
                                                                      (symbol->string (first keys))))])]
                                    [VTglobal () (Vglobal (global-scope parentenv))]))
                       (make-scope-db/recur (rest keys)))])])
      (make-scope-db/recur (hash-keys input)))))

;; Applies a function after building the environment with function parameters and updating
;; the store. Throws exception on function arity error.
;;        
(define (interp-apply [ids : (listof symbol)]       ; function argument symbols, in order
                      [args : (listof CExp)]        ; function argument expressions, in order
                      [aenv : Scope]                ; application environment
                      [cenv : Scope]                ; environment at function definition
                      [vars : FuncVars]             ; vars inside function
                      [body : CExp]                 ; function body 
                      [store : Store]) : CAnswer
  (let ([run-db (make-scope-db vars cenv)])
    (local ([define (interp-apply/recur [ids : (listof symbol)]
                                        [args : (listof CExp)]
                                        [store : Store])
             (cond
               [(empty? ids)
                (begin
                  ; debugging:
                  ;(display "ABOUT TO APPLY WITH:\n")
                  ;(pretty-struct cenv)
                  ;(display "\n----\n")
                  ;(pretty-store store)
                  ;(display "\n----\n")
                  (interp-env body (Slocal run-db cenv) store))]
               [else
                (let ([loc (new-loc)])
                  (type-case CAnswer (interp-env (first args) aenv store)
                    [AObject (a-v a-s)
                             (begin
                               (hash-set! run-db (first ids) (Vlocal loc true))
                               (interp-apply/recur (rest ids)
                                                   (rest args)
                                                   (hash-set a-s loc a-v)))]
                    [AException (exn-v exn-s) (AException exn-v exn-s)]
                    [else (error 'interp-apply "unexpected result")]))])])
      (cond
        [(not (equal? (length ids) (length args)))
         (AException (make-TypeError 
                      (list (make-str 
                             (string-append 
                              "Application failed with arity mismatch: " 
                              (string-append 
                               (number->string (length ids))
                               (string-append
                                " expected vs "
                                (number->string (length args))))))))
                     store)]
        [else (interp-apply/recur ids args store)]))))

;; The interpreter
(define (interp-env [expr : CExp]
                    [env : Scope]
                    [store : Store]) : CAnswer
  (type-case CExp expr    
    ;; primitives    
    [CInt (n)
          (AObject (make-int n) store)]
    
    [CFloat (n)
            (AObject (make-float n) store)]
    
    [CComplex (n)
              (AObject (make-complex n) store)]
    
    [CStr (s)
          (AObject (make-str s) store)]
    
    [CFunc (args vars body)
           (begin
             ;; TODO: python walks the body at function definition time to check that sub-functions
             ;; don't have any invalid nonlocal bindings. we only walk the body at at application time.
             ;; not sure if this is relevant to our test suite, or not.
             ;; 
             (make-scope-db vars env) ; side-effect: check nonlocal bindings at time of definition
             (AObject (make-function env args vars body) store))]

    [CReturn (value)
             (let ([ans (interp-env value env store)])
               (type-case CAnswer ans
                 [AObject (v s) (AReturn v s)]
                 [else ans]))]
    
    [CRaise (exn) 
            (let ([ans (interp-env exn env store)])
              (type-case CAnswer ans
                [AObject (v s) (AException v s)]
                [else ans]))]

    [CIf (i t e)
         (let ([ans (interp-env i env store)])
           (type-case CAnswer ans
             [AObject (v s)
                      (cond
                        [(truth-value v) (interp-env t env s)]
                        [else (interp-env e env s)])]
             [else ans]))]

    [CId (x)
         (let ([ans (lookup x env store)])
           (type-case CAnswer ans
             [ALocation (loc-v loc-s) (fetch loc-v loc-s)]
             [else ans]))]

    [CSetExp (id bind)
             (let ([ans (interp-env bind env store)])
               (type-case CAnswer ans
                 [AObject (val-v val-s) 
                          (AObject val-v
                                   (hash-set val-s 
                                             (find-loc id env)
                                             val-v))]
                 [else ans]))]

    [CSetObj (id bind)
             [AObject bind (hash-set store (find-loc id env) bind)]]
    
    [CDel (x)
          (AObject py-None store)] ; TODO implement delete!!! this is just a placeholder
    
    [CSeq (e1 e2)
          (let ([ans (interp-env e1 env store)])
            (type-case CAnswer ans
              [AObject (e1-v e1-s) (interp-env e2 env e1-s)]
              [else ans]))]

    [CApp (fun arges)
          (let ([fun-ans (interp-env fun env store)])
            (type-case CAnswer fun-ans
              [AObject (f-v f-s) (type-case CObject f-v
                                   [OFunction (class dict closenv argxs vars body)
                                              (let ([app-ans (interp-apply argxs
                                                                           arges
                                                                           env
                                                                           closenv
                                                                           vars
                                                                           body
                                                                           f-s)])
                                                (type-case CAnswer app-ans
                                                  [AReturn (v s) (AObject v s)]
                                                  [AObject (v s) (AObject py-None s)] ; implicit return of "None"
                                                  [else app-ans]))]
                                   ;; TODO: implement functional application call on class types??
                                   ;[OType (class dict bases name) ... ]
                                   [else (AException (make-TypeError (list (make-str "object is not callable")))
                                                     f-s)])]
              [else fun-ans]))]

    [CPrim1 (prim arg) (python-prim1 prim (interp-env arg env store))]))

;;
;; The interpreter entry point. Returns an object value.
;;
(define (interp expr) : CObject
  (begin
    (pretty-struct expr)
    (type-case CAnswer (interp-env (python-lib expr)
                                   (Sglobal (db-mutable-copy (foldl (lambda (x sofar) 
                                                                      (hash-set sofar x (Vlocal (new-loc) false)))
                                                                    (hash (list))
                                                                    (get-library-names python-library))))
                                   (hash (list))) ; The Store
      [AObject (v s) v]
      [AException (v s) (begin
                          (display "PROGRAM EXCEPTION: ")
                          (display (object-repr v))
                          v)]
      [AReturn (v s) (begin
                       (display "INTERNAL ERROR: A RETURN RETURNED FRO INTERP")
                       (make-str "error"))]
      [ALocation (l s) (begin
                         (display "INTERNAL ERROR: A LOCATION RETURNED FROM INTERP")
                         (make-str "error"))])))
