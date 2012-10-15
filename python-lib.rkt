#lang plai-typed

#|

Here is a suggestion for how to implement shared runtime functionality -
write it as core expression forms and use python-lib to wrap your
desugared expressions in an environment that will contain useful
bindings.  For example, this sample library binds `print` to a function
that calls the primitive `print`.

|#

(define-type Lib (CExp -> CExp))

(define print-lambda
  (CFunc (list 'to-print)
    (CPrim1 'print (CId 'to-print))))

(define lib-functions
  (list (cons 'print print-lambda)))

(define (python-lib expr)
  (define (python-lib/recur libs)
    (cond [(empty? libs) expr]
          [(cons? libs)
           (CLet (car (first libs))
                 (cdr (first libs))
                 (python-lib/recur (rest libs)))])))
