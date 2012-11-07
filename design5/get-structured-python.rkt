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
     (PyModule (PySeq (map get-structured-python expr-list)))]
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
    
    ;; Catching None up here, before we hit the identifer case
    [(hash-table ('nodetype "Name")
                 ('ctx _)
                 ('id "None"))
     (PyNone)]
    
    ;; Should these cases be in lib? This is something to ask at design check...   
    ;; Alternatively, they could be handled in desugaring. This is a decision we need to make
    ;; before we can proceed. I vote for desugaring. 
    
    [(hash-table ('nodetype "Name")
                 ('ctx _)        ;; ignoring ctx for now
                 ('id id))
     (PyId (string->symbol id))]
    [(hash-table ('nodetype "Num")
                 ('n n))
     (PyNum n)]
    ;;MADE BY ME:
    [(hash-table ('nodetype "If")
                 ('body body) ;stmt-list
                 ('test test) ;expr
                 ('orelse orelse)) ;stmt-list
     (PyIf (get-structured-python test )
           (map get-structured-python body)
           (map get-structured-python orelse))]
    [(hash-table ('nodetype "BoolOp")
                 ('op op) ;boolop
                 ('values values)) ;expr-list
     (PyBoolop (get-structured-python op) (map get-structured-python values))]
    [(hash-table ('nodetype "UnaryOp")
                 ('op op)
                 ('operand operand))
     (PyUnaryOp (get-structured-python op) (get-structured-python operand))]
    [(hash-table ('nodetype "Compare")
                 ('left left) ;expr
                 ('ops ops) ;cmpop*
                 ('comparators comparators)) ;expr*
     (PyCompare (get-structured-python left)
                (map get-structured-python ops)
                (map get-structured-python comparators))]
    [(hash-table ('nodetype "Str")
                 ('s s))
     (PyStr s)]
    [(hash-table ('nodetype "Pass"))
     (PyPass)]
    
    
    
    [(hash-table ('nodetype "Lambda")
                 ('args args) ;arguments
                 ('body body)) ;expr
     (PyLambda (get-structured-python args)
               (get-structured-python body))]
    [(hash-table ('nodetype "arguments")
                 ('args args) ;arg*
                 ('vararg vararg)
                 ('varargannotation varargannotation)
                 ('kwonlyargs kwonlyargs)
                 ('kwarg kwarg)
                 ('kwargannotation kwargannotation)
                 ('defaults defaults)
                 ('kw_defaults kw_defaults))
     (map get-structured-python args)]
    [(hash-table ('nodetype "arg")
                 ('arg arg)
                 ('annotation annotation))
     (string->symbol arg)]
    [(hash-table ('nodetype "Assign")
                 ('targets targets)
                 ('value value))
     (PyAssign (map get-structured-python targets)
               (get-structured-python value))]
    ;; raise
    [(hash-table ('nodetype "Raise")
                 ('exc exc)
                 ('cause cause))
     (PyRaise (get-structured-python exc))] ;; (get-structured-python cause))]
    [(hash-table ('nodetype "BinOp")
                 ('left left) ;expr
                 ('op op) ;operator
                 ('right right)) ;expr
     (PyBinOp (get-structured-python op)
              (get-structured-python left)
              (get-structured-python right))]
    
    
    ;; def
    [(hash-table ('nodetype "FunctionDef")
                 ('name name)
                 ('args args)
                 ('body body)
                 ('decorator_list decorator-list)
                 ('returns returns))
     (PyDef (string->symbol name) 
            (get-structured-python args)
            (PySeq (map get-structured-python body)))]
    
    ;; return case
    [(hash-table ('nodetype "Return")
                 ('value value))
     (PyReturn (get-structured-python value))]
    
    
    ;; global variable
    [(hash-table ('nodetype "Global")
                 ('names names))
     (PyGlobal (map (lambda (name) (string->symbol name)) names))]
    
    ;; nonlocal variable
    [(hash-table ('nodetype "Nonlocal")
                 ('names names))
     (PyNonlocal (map (lambda (name) (string->symbol name)) names))]
    
                 
    ;;THE ONES THAT RETURN PRIMITIVES (symbols, numbers, strings, etc):
    
    ;; arithmetic
    [(hash-table ('nodetype "Add"))
     'python-add]
    [(hash-table ('nodetype "Sub"))
     'python-sub]
    [(hash-table ('nodetype "Mult"))
     'python-mult]
    [(hash-table ('nodetype "Div"))
     'python-div]
 ;   [(hash-table ('nodetype "Pow")) ;; this will need to be recursive. Wait to implement...
 ;    'python-pow]
    
    [(hash-table ('nodetype "Or"))
     'or]
    [(hash-table ('nodetype "And"))
     'and]
    [(hash-table ('nodetype "Eq"))
     'python-eq]
    [(hash-table ('nodetype "NotEq"))
     'python-notEq]
    [(hash-table ('nodetype "Lt"))
     'python-lt]
    [(hash-table ('nodetype "LtE"))
     'python-lte]
    [(hash-table ('nodetype "Gt"))
     'python-gt]
    [(hash-table ('nodetype "GtE"))
     'python-gte]
    [(hash-table ('nodetype "Is"))
     'python-is]
    [(hash-table ('nodetype "IsNot"))
     'python-isNot]
    [(hash-table ('nodetype "In"))
     'python-in]

    
    ;; Unary
    [(hash-table ('nodetype "Not"))
     'not]
    [(hash-table ('nodetype "USub"))
     'negative]
    [(hash-table ('nodetype "UAdd"))
     'python-uadd]
    [(hash-table ('nodetype "Invert"))
     'python-invert]
    
    
    
                 
    [_ (begin (display "PARSING: Haven't handled a case yet: \n")
              (pretty-write pyjson)
              (error 'parse "error is described above."))]))

