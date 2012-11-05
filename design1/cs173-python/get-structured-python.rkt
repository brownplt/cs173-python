#lang plai

(require "python-syntax.rkt")
(require racket/match
         racket/list)

#|

Python parses as a JSON structure that we export from Python's ast
module.  You should use this file to turn it into a plai-typed data
structure that you define in python-syntax.rkt

|#

(define (get-structured-python pyjson)
  (match pyjson
    [(hash-table ('nodetype "Module") ('body expr-list))
     (PySeq (map get-structured-python expr-list))]
    [(hash-table ('nodetype "Expr") ('value expr))
     (get-structured-python expr)]
    [(hash-table ('nodetype "Call")
                 ('keywords keywords) ;; ignoring keywords for now
                 ('kwargs kwargs)     ;; ignoring kwargs for now
                 ('starargs starargs) ;; ignoring starargs for now
                 ('args args-list)
                 ('func func-expr))
     (PyApp (get-structured-python func-expr)
            (map get-structured-python args-list))]
    [(hash-table ('nodetype "Name")
                 ('ctx _)        ;; ignoring ctx for now
                 ('id id))
     (PyId (string->symbol id))]
    [(hash-table ('nodetype "Num")
                 ('n n))
     (PyNum n)]
    
    
    [(hash-table ('nodetype "BoolOp")
                 ('values values)
                 ('op op))
     (PyBoolOp (get-structured-python op) (map get-structured-python values))]
    
    [(hash-table ('nodetype "BinOp")
                 ('right right)
                 ('left left)
                 ('op op))
     (PyBinOp (get-structured-python op) (get-structured-python left) (get-structured-python right))]
    
    [(hash-table ('nodetype "UnaryOp")
                 ('op op)
                 ('operand arg))
     (PyPrimOp (get-structured-python op) (get-structured-python arg))]
    
    [(hash-table ('nodetype "Or"))
     'or]
    [(hash-table ('nodetype "And"))
     'and]
    [(hash-table ('nodetype "Not"))
     'not]
    [(hash-table ('nodetype "Lt"))
     'lt]
    [(hash-table ('nodetype "LtE"))
     'lte]
    [(hash-table ('nodetype "Eq"))
     'eq]
    [(hash-table ('nodetype "GtE"))
     'gte]
    [(hash-table ('nodetype "Gt"))
     'gt]
    [(hash-table ('nodetype "NotEq"))
     'neq]
    [(hash-table ('nodetype "Is"))
     'is]
    [(hash-table ('nodetype "Add"))
     'add]
    [(hash-table ('nodetype "Sub"))
     'sub]
    [(hash-table ('nodetype "Mult"))
     'mul]
    [(hash-table ('nodetype "FloorDiv"))
     'floordiv]
    [(hash-table ('nodetype "Div"))
     'div]
    [(hash-table ('nodetype "Mod"))
     'mod]
    [(hash-table ('nodetype "In"))
     'in]
    [(hash-table ('nodetype "NotIn"))
     'nin]
    
    [(hash-table ('nodetype "If")
                 ('test test)
                 ('body body)
                 ('orelse orelse))
     (PyIf (get-structured-python test)
           (PySeq (map get-structured-python body))
           (PySeq (map get-structured-python orelse)))]
    
    [(hash-table ('nodetype "Compare")
                 ('ops ops)
                 ('comparators comparators)
                 ('left left))
     (PyComp (get-structured-python left) 
             (map get-structured-python ops)
             (map get-structured-python comparators))]
    
    [(hash-table ('nodetype "Pass"))
     (PyPass)]
    
    [(hash-table ('nodetype "Raise")
                 ('cause cause)
                 ('exc exn))
     (PyRaise (get-structured-python exn))]
    
    [(hash-table ('nodetype "Str")
                 ('s string))
     (PyStr string)]
    
    [(hash-table ('nodetype "Assign")
                 ('targets lhs)
                 ('value value))
     (PyAssign (get-structured-python (first lhs)) (get-structured-python value))]
    
    [(hash-table ('nodetype "Lambda")
                 ('body body)
                 ('args args))
     (PyLambda (get-structured-python args) (get-structured-python body))]
    
    ;;this case will need to be developed more fully as we start to incorporate
    ;;default arg values, and other cool stuff.
    [(hash-table ('nodetype "arguments")
                 ('defaults defaults)
                 ('kwargannotation kwann)
                 ('vararg vararg)
                 ('args arg-list)
                 ('kwarg kwarg)
                 ('varargannotation varann)
                 ('kw_defaults kw_defaults)
                 ('kwonlyargs kwonly))
     (map get-structured-python arg-list)]
    
    [(hash-table ('nodetype "arg")
                 ('annotation annotation)
                 ('arg name))
     (string->symbol name)]
    [(hash-table ('nodetype "List")
                 ('elts elts)
                 ('ctx ctx))
     (PyList #t (map get-structured-python elts))]
    
    
    [_ (begin (display pyjson) (error 'parse "Haven't handled a case yet"))]))

