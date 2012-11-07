#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt")

;; cascade-lets will build up the nested lets, and use body as the
;; eventual body, preserving order of evaluation of the expressions
(define (cascade-lets (ids : (listof symbol))
                      (sts : (listof ScopeType))
                      (exprs : (listof CExp))
                      (body : CExp)) : CExp
  (cond [(empty? ids) body]
        [(cons? ids)
         (CLet (first ids) (first sts) (first exprs) (cascade-lets (rest ids) (rest sts) (rest exprs) body))]))


(define (get-vars [expr : PyExpr]) : (listof (ScopeType * symbol))
  (type-case PyExpr expr
    [PyNonlocal (ids) (map (lambda (id) (values (NonLocal) id)) ids)]
    [PyGlobal (ids) (map (lambda (id) (values (Global) id)) ids)]
    [PyGlobalEnv () (list)]
    [PySeq (es) (foldl (lambda (a b) (append b a))
                       (list)
                       (map (lambda (e) (get-vars e)) es))]
    [PyNum (n) (list)]
    [PyApp (f args) (append (get-vars f)
                            (foldl (lambda (a b) (append b a))
                                   (list)
                                   (map (lambda (e) (get-vars e)) args)))]
    [PyReturn (value) (list)]
    [PyId (id) (list)]
    [PyStr (s) (list)]
    [PyBinOp (op left right)
             (append
              (get-vars left)
              (get-vars right))]
    [PyIf (test then orelse)
          (append
           (get-vars test)
           (append
            (foldl (lambda (a b) (append b a))
                   (list)
                   (map (lambda (e) (get-vars e)) then))
            (foldl (lambda (a b) (append b a))
                   (list)
                   (map (lambda (e) (get-vars e)) orelse))))]
    [PyBoolop (op exprs)
              (foldl (lambda (a b) (append b a))
                                   (list)
                                   (map (lambda (e) (get-vars e)) exprs))]
    [PyCompare (left ops comparators)
               (append
                (get-vars left)
                (foldl (lambda (a b) (append b a))
                                   (list)
                                   (map (lambda (e) (get-vars e)) comparators)))]
    [PyPass () (list)]
    [PyNone () (list)]
    [PyLambda (args body) (list)]
    [PyDef (name args body)
           (list (values (Local) name))]
    
    [PyRaise (exc) (get-vars exc)]
    [Py-NotExist () (list)]
    [PyUnaryOp (op arg) (get-vars arg)]
    [PySet (lhs value) ;;PySet case may need to change, because it never actually appears since it only exists from use in PyAssign
           (append
               (get-vars value)
               (type-case PyExpr lhs
                 [PyId (id) (list (values (Local) id))]
                 [else (error 'get-vars-PySet "PySet should not be getting non-ids yet")]))]
    [PyAssign (targets value)
              (append
               (get-vars value)
               (foldl (lambda (a b) (append b a))
                      (list)
                      (map (lambda (e) (type-case PyExpr e
                                         [PyId (id) (list (values (Local) id))]
                                         [else (error 'get-vars-PyAssign "PyAssign should not be getting non-ids yet")])) 
                           targets)))]
    [PyModule (exprs)
              (get-vars exprs)]
    
    ;[else (error 'get-vars "Case not implemented")]
    ))



(define (desugar expr)
  (type-case PyExpr expr
 ;   #|
    [PySeq (es) (foldl (lambda (e1 e2) (CSeq e2 (desugar e1))) (desugar (first es)) (rest es))]
    [PyNum (n) (CNum n)]
    [PyApp (f args) (CApp (desugar f) (map desugar args))]
    [PyId (x) (CId x)]
    ;;Under this, Non-TA code
    [PyStr (s) (CStr s)]
    [PyIf (test then orelse)
          (CIf (desugar test) 
               (if (> (length then) 1) (desugar (PySeq then)) (desugar (first then)))
               (if (> (length orelse) 0) 
                   (if (> (length orelse) 1)
                       (desugar (PySeq orelse)) 
                       (desugar (first orelse)))
                   (CPass)))]
    [PyGlobal (ids) (CPass)]
    [PyNonlocal (ids) (CPass)]
    [PyBoolop (op exprs)
              (case op
                ['or (foldl (lambda (expr result) (CPrim2 'or result (desugar expr))) (desugar (first exprs)) (rest exprs))]
                ['and (foldl (lambda (expr result) (CPrim2 'and result (desugar expr))) (desugar (first exprs)) (rest exprs))])]
    [PyUnaryOp (op arg)
               (CPrim1 op (desugar arg))] ;; TODO this needs to desugar to a function application
    [PyBinOp (op left right)
             (CApp (CId op) (list (desugar left) (desugar right)))]
    [PyCompare (left ops comparators)
               (if (equal? 0 (length comparators))
                   (CTrue)
                   (CLet 'left-comp (Local) (desugar left)
                     (CLet 'right-comp (Local) (desugar (first comparators))
                       (CIf (CApp (CId (first ops))
                                    (list (CId 'left-comp) (CId 'right-comp)))
                            (desugar (PyCompare (PyId 'right-comp)
                                                (rest ops)
                                                (rest comparators)))
                            (CFalse)))))]
    
    [PyPass () (CPass)]
    [PyNone () (CNone)]
    [PyLambda (args body) (CFunc args (desugar body) (list))]
    
    [PyRaise (exc) (CError (desugar exc))]
    [PyAssign (targets value) 
              (CLet 'assign-value (Local) (desugar value)
                    (desugar (PySeq (map (lambda (e) (PySet e (PyId 'assign-value))) targets))))]
    [PySet (lhs value) 
           (CSet (desugar lhs) (desugar value))]
    [PyGlobalEnv () (CGlobalEnv)]
    [PyModule (exprs) 
              (let ([global-vars (get-vars exprs)]) ;gets all of the assignments in the global scope
                (begin (if (hasGlobalScopeErrors global-vars) ;checks the existence of 'nonlocal' or 'global' declarations in the global scope
                           (error 'PyModule "Global or Nonlocal declaration in the global scope.")
                           (void))
                       (cascade-lets (get-ids global-vars) ;puts the variables in the environment as Globals
                                     (make-item-list (Global) (length global-vars) (list)) 
                                     (make-item-list (CUnbound) (length global-vars) (list)) 
                                     (desugar (PySeq (append 
                                                      (list (PyGlobalEnv)) ;the first thing interpreter does is creating the 
                                                                           ;separate global environment
                                                      (list exprs)))))))] ;executes the program
    
    [PyDef (name args body) 
           (begin (CSeq
                   (CSet (CId name) (CFunc (list) (CError (CStr "dummy function was called!")) (list)))
                   (CLet 'some-func (Local) (CFunc args (desugar body) (get-vars body))
                         (CSet (CId name) (CId 'some-func)))))]
                

    
;|#
    [else (error 'desugar (string-append "Haven't desugared a case yet:\n"
                                       (to-string expr)))]))

(define (make-item-list [item : 'a]
                        [size : number]
                        [newList : (listof 'a)]) : (listof 'a)
  (cond 
    [(>= (length newList) size) newList]
    [else (make-item-list item size (append (list item) newList))]))

(define (get-ids [vars-list : (listof (ScopeType * symbol))]) : (listof symbol)
  (foldl (lambda (a b) (append b a))
                       (list)
                       (map (lambda (e) (local ([define-values (st id) e])
                                                      (list id)))
                              vars-list)))

(define (get-sts [vars-list : (listof (ScopeType * symbol))]) : (listof ScopeType)
  (foldl (lambda (a b) (append b a))
                       (list)
                       (map (lambda (e) (local ([define-values (st id) e])
                                                      (list st)))
                              vars-list)))


;; hasGlobalScopeErrors checks the declarations in the global environment.
;; If we have something that is not a 'Local', we return true ('we have an error').
(define (hasGlobalScopeErrors [vars : (listof (ScopeType * symbol))]) : boolean
  (not (foldl (lambda (list-el result) (and list-el result))
              true
              (map (lambda (e) (local ([define-values (st id) e])
                                 (Local? st)))
                   vars))))


;(test (desugar (PyBoolop 'or (list (PyNum 0) (PyNum 1))))
;      (CBoolop 'or (CNum 0) (CNum 1)))