#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt")

(require (typed-in racket (number->string : (number -> string))))
(require (typed-in racket (integer? : ('_a -> boolean))))
(require (typed-in racket (flonum? : ('_a -> boolean))))

;; Gets all the vars in the sub-expression, using get-vars-inner. It
;; takes a (possibly empty) list of function parameters as 'args'. These
;; will get mapped into the scope before the expression is walked. Also,
;; this list will be checked for duplicates: a syntax error is raised if
;; duplicates exist
(define (get-vars [args : (listof symbol)]
                  [expr : PyExpr]) : FuncVars
  (get-vars-inner expr (foldl (lambda (x sofar)
                                (type-case (optionof VarType) (hash-ref sofar x)
                                  [some (v) (begin
                                              (display "SyntaxError: invalid syntax, duplicate identifiers in function definition")
                                              (error 'get-vars "goodbye"))]
                                  [none () (hash-set sofar x (VTlocal true))]))
                              (hash (list))
                              args)))

;; Gets all the vars in a (listof PyExpr) of expressions using get-vars-inner
(define (get-vars-list [loe : (listof PyExpr)]
                       [vars : FuncVars]) : FuncVars
  (cond
    [(empty? loe) vars]
    [else (get-vars-list (rest loe)
                         (get-vars-inner (first loe) vars))]))

;; Adds a variable that has been assigned to. This means it should be a
;; local variable, UNLESS it has been previously declared as nonlocal
;; or global. In that case, we preserve the nonlocal/global declaration.
(define (add-local [id : symbol]
                   [vars : FuncVars]) : FuncVars
  (type-case (optionof VarType) (hash-ref vars id)
    [some (v) 
          vars] ;; do nothing; our var is already defined in a scope.
                ;; assignment does not change the current definition.
    [none () (hash-set vars id (VTlocal false))]))

;; Adds a variable that has a 'global' declaration. Errors if the variable is
;; already defined as 'nonlocal'. Warns if a variable was previously assigned,
;; but promotes that variable to global scope anyway.
(define (add-global [id : symbol]
                    [vars : FuncVars]) : FuncVars
  (type-case (optionof VarType) (hash-ref vars id)
    [some (v) 
          (type-case VarType v
            [VTlocal (isparam) 
                    (begin
                      (display "SyntaxWarning: name '")
                      (display (symbol->string id))
                      (if isparam
                          (display "' is parameter and global")
                          (display "' is assigned to before global declaration"))
                      (hash-set vars id (VTglobal)))]
            [VTnonlocal ()                     
                       (begin
                         (display "SyntaxError: name '")
                         (display (symbol->string id))
                         (display "' is non-local and global")
                         (error 'add-global "goodbye"))]
            [VTglobal () 
                     vars])]
    [none () (hash-set vars id (VTglobal))]))


;; Adds a variable that has a 'nonlocal' declaration. Errors if the variable is
;; already defined as 'global'. Warns if a variable was previously assigned,
;; but promotes that variable to global scope anyway.
(define (add-nonlocal [id : symbol]
                      [vars : FuncVars]) : FuncVars
  (type-case (optionof VarType) (hash-ref vars id)
    [some (v) 
          (type-case VarType v
            [VTlocal (isparam) 
                    (begin
                      (display "SyntaxWarning: name '")
                      (display (symbol->string id))
                      (if isparam
                          (display "' is parameter and nonlocal")
                          (display "' is assigned to before non-local declaration"))
                      (hash-set vars id (VTglobal)))]
            [VTglobal ()                     
                       (begin
                         (display "SyntaxError: name '")
                         (display (symbol->string id))
                         (display "' is non-local and global")
                         (error 'add-nonlocal "goodbye"))]
            [VTnonlocal () 
                     vars])]
    [none () (hash-set vars id (VTnonlocal))]))

;; Helper to get all the vars in the sub-expression, stopping at Func/Lambda/Class?
(define (get-vars-inner [expr : PyExpr]
                        [vars : FuncVars]) : FuncVars
  (type-case PyExpr expr
    [PyGlobal (names)
              (foldl add-global vars names)]
    
    [PyNonlocal (names)
             (foldl add-nonlocal vars names)]
    
    [PySeq (es)
           (get-vars-list es vars)]
    
    [PyAssign (value targets)
              (get-vars-inner value (foldl add-local vars targets))]
              
    [PyReal (n)
            vars]
    
    [PyStr (s) 
           vars]
    
    [PyComplex (n)
               vars]
        
    [PyFunc (id args body) ; assignment will occur on 'id'
            (add-local id vars)]
    
    [PyReturn (value)
              (get-vars-inner value vars)]
    
    [PyId (x)
          vars]
    
    [PyIf (tst thn els) ; may have assignment operations
          (get-vars-inner tst (get-vars-inner thn (get-vars-inner els vars)))]
 
    [PyApp (fun args)
           (get-vars-inner fun (get-vars-list args vars))]
    
    [PyPass ()
            vars]
    
    ))

;;
;; Create unique temporary var name. Note that
;; temporary vars have a '-' in them to prevent
;; collisions with user variables.
;;
;; WARNING: this collision avoidance scheme only
;; works with variables, NOT with function 
;; attributes!
;;
(define new-var
  (let ([n (box 0)])
    (lambda () : symbol
      (begin
        (set-box! n (add1 (unbox n)))
        (string->symbol (string-append "tmp-" (number->string (unbox n))))))))

;;
;; Desugar, starting a new scope
;;
(define (desugar expression)
  ;; Desugar, in same scope
  (local [(define (desugar/recur expr)
            (type-case PyExpr expr
              [PyGlobal (names)
                        (CId 'None)]
              
              [PyNonlocal (names)
                          (CId 'None)]
              
              [PySeq (es)
                     (foldl (lambda (e1 e2)
                              (CSeq e2 (desugar/recur e1)))
                            (desugar/recur (first es))
                            (rest es))]
                            
              [PyAssign (value targets)
                        (let ([var (new-var)])
                          (CSeq (foldl (lambda (x sofar)
                                         (CSeq sofar (CSetExp x (CId var))))
                                       (CSetExp var (desugar/recur value))
                                       targets)
                                (CDel var)))]
                                         
              [PyStr (s)
                     (CStr s)]
              
              [PyFunc (id args body)
                      (CSetExp id 
                               (CFunc args (get-vars args body)
                                 (desugar/recur body)))]
              
              [PyReturn (value)
                        (CReturn (desugar/recur value))]
              
              [PyReal (n) (cond
                            [(integer? n) (CInt n)]
                            [(flonum? n) (CFloat n)])]
              
              [PyIf (tst thn els)
                    (CIf (desugar/recur tst)
                         (desugar/recur thn)
                         (desugar/recur els))]
              
              [PyComplex (n)
                         (CComplex n)]
              
              [PyApp (f args)
                     (CApp (desugar/recur f)
                           (map desugar/recur args))]
              
              [PyPass ()
                      (CId 'None)]
              
              [PyId (x)
                    (CId x)]))]
    (desugar/recur expression)))
  