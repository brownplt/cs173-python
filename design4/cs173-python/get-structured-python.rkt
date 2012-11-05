#lang plai

(require "python-syntax.rkt")
(require racket/match
         racket/list)

#|

Python parses as a JSON structure that we export from Python's ast
module.  You should use this file to turn it into a plai-typed data
structure that you define in python-syntax.rkt

|#
;; get-structured-python (gsp) converts json to PyExprs
(define (gsp pyjson)
  (match pyjson
    [(hash-table ('nodetype "Module") ('body expr-list))
     (PySeq (map gsp expr-list))]
    [(hash-table ('nodetype "Expr") ('value expr))
     (gsp expr)]
    [(hash-table ('nodetype "Pass")) (PyPass)]
    [(hash-table ('nodetype "Call")
                 ('keywords keywords) ;; ignoring keywords for now
                 ('kwargs kwargs)     ;; ignoring kwargs for now
                 ('starargs starargs) ;; ignoring starargs for now
                 ('args args-list)
                 ('func func-expr))
     (PyApp (gsp func-expr)
            (map gsp args-list))]
    [(hash-table ('nodetype "Name")
                 ('ctx _) ;; ctx isn't relevant for None, I think.
                 ('id "None"))
     (PyNone)]
    [(hash-table ('nodetype "Name")
                 ('ctx ctx)
                 ('id id))
     (PyId (string->symbol id))]
    [(hash-table ('nodetype "Num")
                 ('n n))
     (PyNum n)]
    [(hash-table ('nodetype "Str")
                 ('s s))
     (PyStr s)]
    [(hash-table ('nodetype "If")
                 ('test test-expr)
                 ('body body-expr)
                 ('orelse orelse-expr))
     (PyIf (gsp test-expr)
           (gsp body-expr)
           (gsp orelse-expr))]
    [(hash-table ('nodetype "BoolOp")
                 ('op (hash-table ('nodetype op)))
                 ('values val-list))
     (PyBoolOp (string->symbol op)
               (gsp (first val-list))
               (gsp (second val-list)))]
    [(hash-table ('nodetype "BinOp")
                 ('op op)
                 ('left l)
                 ('right r))
     (PyBinOp (string->symbol (hash-ref op 'nodetype)) (gsp l) (gsp r))]
    [(hash-table ('nodetype "UnaryOp")
                 ('op (hash-table ('nodetype op)))
                 ('operand val))
     (PyUnaryOp (string->symbol op)
                (gsp val))]
    [(hash-table ('nodetype "Dict")
                 ('keys ks)
                 ('values vs))
     (let ([hs (make-hash)])
        (begin
         (map (lambda (k v) (hash-set! hs (gsp k)
                                          (gsp v)))
               ks vs)
         (PyDict hs)))]
    [(hash-table ('nodetype "Compare")
                 ('ops oplist)
                 ('comparators cmps)
                 ('left lft))
     (PyCompare (map (lambda (h) (string->symbol (hash-ref h 'nodetype))) oplist)
                (map gsp cmps)
                (gsp lft))]
    [(hash-table ('nodetype "Lambda")
                 ('args args)
                 ('body body))
     (PyLambda (get-structured-args args) (gsp body))]
    [(hash-table ('nodetype "FunctionDef")
                 ('name name)
                 ('decorator_list _)
                 ('args args)
                 ('returns _) ;; ?: what is returns?
                 ('body body))
     (PyFunDef (string->symbol name) (get-structured-args args) (gsp body))]
    [(hash-table ('nodetype "Return")
                 ('value value))
     (PyReturn (gsp value))]
    [(hash-table ('nodetype "Assign")
                 ('targets targets)
                 ('value value))
     (PyAssign (map (lambda (h) (PyId (string->symbol (hash-ref h 'id))))
                    targets) (gsp value))]
    [(hash-table ('nodetype "While")
                 ('orelse _)
                 ('test tst)
                 ('body body))
     (PyWhile (gsp tst) (gsp body))]
    [(hash-table ('nodetype "Break")) (PyBreak)]
    [(hash-table ('nodetype "Continue")) (PyContinue)]
    ;; an empty list of expressions is a pass, a non-empty list
    ;; is the sequence of expressions
    [(list) (PyPass)]
    [(cons f rs) (PySeq (map gsp pyjson))]
    [_ ;(begin (display pyjson)
       ;       (display "\n")
              (PyUndefined)]))

(define (get-structured-args argjson)
    (match argjson
        [(hash-table ('nodetype "arguments")
                     ('args args)
                     ('defaults defs)
                     ('kwarg _)
                     ('kwargannotation _)
                     ('vararg _)
                     ('varargannotation _)
                     ('kw_defaults _)
                     ('kwonlyargs _))
        (PyArgs (map (lambda (h) (string->symbol (hash-ref h 'arg))) args)
                (map gsp defs))]))
