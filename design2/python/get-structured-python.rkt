#lang plai

(require "python-syntax.rkt")
(require racket/match
         racket/list)

#|

Python parses as a JSON structure that we export from Python's ast
module.  You should use this file to turn it into a plai-typed data
structure that you define in python-syntax.rkt

|#

(define (get-assign-target pyjson)
  (match pyjson
    [(hash-table ('nodetype "Name")
                 ('ctx _)        ;; ignoring ctx for now
                 ('id id))
     (string->symbol id)]))


(define (get-structured-python pyjson)
  (match pyjson
    ; the default for a list is a sequence of expressions;
    ; note that this is not true in some cases (such as function arguments)
    [(list expr-list ...)
     (PySeq (map get-structured-python expr-list))]
    [(hash-table ('nodetype "Module")
                 ('body expr-list))
     ; note that we could have Module produce a PyModule which starts the 
     ; global scope. TODO: this about this.
     (get-structured-python expr-list)]
    [(hash-table ('nodetype "arg")
                 ('arg arg)
                 ('annotation annotation)) ;; ignoring for now
     (string->symbol arg)]
    [(hash-table ('nodetype "arguments")
                 ('args args)
                 ('defaults defaults) ;; ignoring for now
                 ('kwargannotation kwargannotation) ;; ignoring for now
                 ('vararg vararg) ;; ignoring for now
                 ('kwarg kwarg)  ;; ignoring for now
                 ('varargannotation varargannotation) ;; ignoring for now
                 ('kw_defaults kwdefaults) ;; ignoring for now
                 ('kwonlyargs kwonlyargs)) ;; ignoring for now
     (map get-structured-python args)]
    [(hash-table ('nodetype "FunctionDef")
                 ('name name)
                 ('args args)
                 ('body body)
                 ('decorator_list dec) ;; ignoring for now
                 ('returns returns)) ;; ignoring for now
     (PyFunc (string->symbol name)
             (get-structured-python args)
             (get-structured-python body))]
    [(hash-table ('nodetype "Return")
                 ('value value))
     (PyReturn (get-structured-python value))]
    [(hash-table ('nodetype "Pass"))
     (PyPass)]
    [(hash-table ('nodetype "Expr")
                 ('value expr))
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
    [(hash-table ('nodetype "Assign")
                 ('value value)
                 ('targets targets))
     (PyAssign (get-structured-python value)
               (map get-assign-target targets))]
    [(hash-table ('nodetype "Str")
                 ('s s))
     (PyStr s)]
    [(hash-table ('nodetype "If")
                 ('test tst)
                 ('body thn)
                 ('orelse els))
     (PyIf (get-structured-python tst)
           (get-structured-python thn)
           (get-structured-python els))]
    [(hash-table ('nodetype "Num")
                 ('n n))
     (match n
       [(hash-table ('nodetype "Complex")
                    ('value cn))
        (begin
          (display "GOT COMPLEX: ")
          (display cn)
          (display "\n")
          (PyComplex (string->number (cond
                                       [(equal? (string-replace cn "+" "*") cn)
                                        (string-replace cn "j" "i")]
                                       [else (string-append "0+" (string-replace cn "j" "i"))]))))]
       [else (PyReal n)])]
    [(hash-table ('nodetype "Nonlocal")
                 ('names names)) 
     (PyNonlocal (map string->symbol names))]
    [(hash-table ('nodetype "Global")
                 ('names names))
     (PyGlobal (map string->symbol names))]
    [_ (begin
         (pretty-write pyjson)
         (error 'parse "Haven't handled above case yet"))]))

