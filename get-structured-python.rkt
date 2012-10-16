#lang plai

(require "python-syntax.rkt")
(require racket/match
         racket/list)

#|

Python parses as an s-expression full of nested expression and statement
types that only provide structure for the syntax, and don't have actual
meaning as expressions.  It is up to you to filter through them and get
to the actual expressions that matter.  We've given enough to get
through a few simple programs.

Note that many of these don't always fall through, and can have more
than one sub-expression, like and_test and or_test, which have multiple
arguments if you parse the expression "5 and 3", for example.

|#

(define (filter-cruft s-exp)
  (match s-exp
    [(or (list 'file_input e "")
         (list 'stmt e)
         (list 'simple_stmt e "\n")
         (list 'small_stmt e)
         (list 'expr_stmt e)
         (list 'testlist e)
         (list 'test e)
         (list 'or_test e)
         (list 'and_test e)
         (list 'not_test e)
         (list 'comparison e)
         (list 'expr e)
         (list 'xor_expr e)
         (list 'and_expr e)
         (list 'shift_expr e)
         (list 'arith_expr e)
         (list 'term e)
         (list 'factor e)
         (list 'power e)
         (list 'testlist_comp e))
     (filter-cruft e)]
    [_ s-exp]))

(define (get-structured-python s-exp)
  (local [(define filtered (filter-cruft s-exp))]
  (match filtered
    [`(print_stmt "print" ,e)
     (PyApp (PyId 'print) (list (get-structured-python e)))]
    ;; singleton tuple case, you will need to handle the multi-tuple case
    [`(atom "(" ,e ")") (get-structured-python e)]
    [`(atom ,e) (parse-atom e)]
    [_ (error 'parse (format "Haven't handled a case yet: ~a" s-exp))])))

#|

The grammar defines a number of "atoms", which are things like strings,
booleans, numbers, and identifiers.  They are all represented as
different kinds of strings - e.g. strings look like "'a string'",
numbers look like "45", and identifiers look like "x".  You'll need to
figure out how to handle all the cases.

Disclaimer: This certainly doesn't cover all cases.

|#
(define (parse-atom e)
  (cond
    ;; NOTE: string->number returns false for non-numeric strings
    [(string->number e) (PyNum (string->number e))]
    ;; more cases for detecting string literals, identifiers, etc.
  ))

