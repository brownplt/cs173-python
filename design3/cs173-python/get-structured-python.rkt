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
                 ('starargs starargs)
                 ('args args-list)
                 ('func func-expr))
     (PyApp (get-structured-python func-expr)
            (map get-structured-python args-list)
            (if (equal? starargs #\nul)
                (PyTuple empty)
                (get-structured-python starargs)))]
    [(hash-table ('nodetype "Name")
                 ('ctx (hash-table ('nodetype "Load")))
                 ('id id))
     (PyId (string->symbol id))]
    [(hash-table ('nodetype "Assign")
                 ('targets vars)
                 ('value value))
     (PySet! (get-structured-python (first vars))
             (get-structured-python value))]
    [(hash-table ('nodetype "Name")
                 ('ctx (hash-table ('nodetype "Store")))
                 ('id id))
     (string->symbol id)]
    [(hash-table ('nodetype "Num")
                 ('n n))
     (PyNum n)]
    [(hash-table ('nodetype "arguments")
                 ('args args)
                 ('defaults defaults)
                 ('kwargannotation kwan)
                 ('vararg va)
                 ('kwarg kw)
                 ('varargannotation vaa)
                 ('kw_defaults kwd)
                 ('kwonlyargs kwoa))
     (values (map get-structured-python args)
             (if (equal? va #\nul)
                 (noneF)
                 (someF (string->symbol va))))]
    [(hash-table ('nodetype "FunctionDef")
                 ('name name)
                 ('args args)
                 ('body body)
                 ('decorator_list dl)
                 ('returns ret))
     (local [(define-values (va n-args) (get-structured-python args))]
            (PySet! (string->symbol name)
                    (PyFunc va n-args
                            (PySeq (map get-structured-python body)))))]
    [(hash-table ('nodetype "Lambda")
                 ('args args)
                 ('body body))
     (local [(define-values (va n-args) (get-structured-python args))]
            (PyFunc va n-args
                    (PyReturn (get-structured-python body))))]
    [(hash-table ('nodetype "arg")
                 ('arg id)
                 ('annotation an))
     (string->symbol id)]
    [(hash-table ('nodetype "Return")
                 ('value value))
     (PyReturn (get-structured-python value))]
    [(hash-table ('nodetype "If")
                 ('test test)
                 ('body body)
                 ('orelse (list)))
     (PyIf (get-structured-python test)
           (PySeq (map get-structured-python body))
           (PyId 'None))]
    [(hash-table ('nodetype "If")
                 ('test test)
                 ('body body)
                 ('orelse else))
     (PyIf (get-structured-python test)
           (PySeq (map get-structured-python body))
           (PySeq (map get-structured-python else)))]
    [(hash-table ('nodetype "BinOp")
                 ('op (hash-table ('nodetype op)))
                 ('left left)
                 ('right right))
     (PyOp (string->symbol op)
           (list (get-structured-python left)
                 (get-structured-python right)))]
    [(hash-table ('nodetype "UnaryOp")
                 ('op (hash-table ('nodetype op)))
                 ('operand operand))
     (PyOp (string->symbol op)
           (list (get-structured-python operand)))]
    [(hash-table ('nodetype "Tuple")
                 ('ctx ctx)
                 ('elts elts))
     (PyTuple
      (map get-structured-python elts))]
    [(hash-table ('nodetype "Pass"))
     (PyPass)]
    [_ (error 'parse (string-append "Haven't handled a case yet:\n"
                                    (format "~s" pyjson)))]))

