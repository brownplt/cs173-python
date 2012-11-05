#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt"
         "python-objects.rkt")

(define (str->cval (str : string)) : CExp
  (%to-object (VStr str)))

(define (call-method (obj : CExp) (name : CExp) (args : (listof CExp)))
  (CLet 'self obj
        (CApp (CGetField (CId 'self)
                         name)
              (append (list (CId 'self))
                      args))))

(define (desugar (expr : PyExpr)) : CExp
  (type-case PyExpr expr
    [PySeq (es) (foldl (lambda (e1 e2) (CSeq e2 (desugar e1))) (desugar (first es)) (rest es))]
    [PyNum (n) (%to-object (VNum n))]
    
    [PyStr (s) (%to-object (VStr s))]
    
    [PyRaise (exn) (CError (desugar exn))]
    
    [PyPass () (CPass)]
    
    [PyLambda (args body) (CFunc args (desugar body))]
    
    [PyAssign (lhs value) (type-case PyExpr lhs
                            [PyId (id) (CSet! id (desugar value))]
                            [else (error 'desugar "So-far invalid assignment")])]
    
    [PyIf (test body orelse) (CIf (desugar test)
                                  (desugar body)
                                  (desugar orelse))]
    [PyList (mutable elts) (CList mutable (map desugar elts))]
    [PyPrimOp (op arg) (CLet 'arg-val (desugar arg)
                             (CApp (CGetField (CId 'arg-val) 
                                              (get-prim-func op))
                                   (list (CId 'arg-val))))]
    
    [PyBinOp (op left right) (call-method (desugar left) (get-prim-func op) (list (desugar right)))]
    
    [PyBoolOp (op values) (cond
                            [(equal? op 'and) (and-op (map desugar values))]
                            [(equal? op 'or) (or-op (map desugar values))])]
    
    [PyComp (left ops comps) (CLet 'left-val (desugar left)
                                   (compare (CId 'left-val) ops (map desugar comps)))] ;;clueless for this one
    
    
    [PyApp (f args) (CApp (desugar f) (map desugar args))]
    [PyId (x) (CId x)]
    [else (error 'desugar (string-append "Haven't desugared yet: "
                                         (to-string expr)))]))

;;This is where we map out the comparison operations to properly short-circuit
;;Assumes the left value is an already-bound CId.
(define (compare (left : CExp) (ops : (listof symbol)) (rights : (listof CExp))) : CExp
  (cond
    [(equal? 1 (length rights)) (CApp (CGetField left
                                                 (get-prim-func (first ops)))
                                      (list left
                                            (first rights)))]
  [else (CLet 'right-val (first rights)
              (CIf (CApp (CGetField left
                                    (get-prim-func (first ops)))
                         (list left
                               (CId 'right-val)))
                   (compare (CId 'right-val) (rest ops) (rest rights))
                   (CId 'False)))]))

(define (and-op (vals : (listof CExp))) : CExp
  (cond
    [(equal? 1 (length vals)) (first vals)]
    [(equal? 2 (length vals)) (CLet 'left (first vals)
                                    (CApp (CGetField (CId 'left)
                                                     (get-prim-func 'and))
                                          (list (CId 'left)
                                                (second vals))))]
    [else (CLet 'front (CLet 'left (first vals)
                             (CApp (CGetField (CId 'left)
                                              (get-prim-func 'and))
                                   (list (CId 'left)
                                         (second vals))))
                (CIf (CId 'front)
                     (and-op (rest (rest vals)))
                     (CId 'False)))]))

(define (or-op (vals : (listof CExp))) : CExp
  (cond
    [(equal? 0 (length vals)) (error 'or-op "wtf mate?")]
    [(equal? 1 (length vals)) (first vals)]
    [(equal? 2 (length vals)) (CLet 'left (first vals)
                                    (CApp (CGetField (CId 'left)
                                                     (get-prim-func 'or))
                                          (list (CId 'left)
                                                (second vals))))]
    [else (CLet 'front (CLet 'left (first vals)
                             (CApp (CGetField (CId 'left)
                                              (get-prim-func 'and))
                                   (list (CId 'left)
                                         (second vals))))
                (CIf (CId 'front)
                     (CId 'True)
                     (or-op (rest (rest vals)))))]))
                

(define (get-prim-func op)
  (%to-object (VStr
               (cond
                 [(equal? op 'add) "%add"]
                 [(equal? op 'mul) "%mul"]
                 [(equal? op 'sub) "%sub"]
                 [(equal? op 'div) "%div"]
                 [(equal? op 'floordiv) "%floordiv"]
                 
                 [(equal? op 'or) "%or"]
                 [(equal? op 'and) "%and"]
                 [(equal? op 'not) "%not"]
                 
                 [(equal? op 'is) "%is"]
                 
                 [(equal? op 'lt) "%lt"]
                 [(equal? op 'lte) "%le"]
                 [(equal? op 'gt) "%gt"]
                 [(equal? op 'gte) "%ge"]
                 [(equal? op 'neq) "%ne"]
                 [(equal? op 'eq) "%eq"]
                 [else (error 'get-prim-func 
                              (string-append "Unrecognized primitive operation: " 
                                             (symbol->string op)))]
     
))))


