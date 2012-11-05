#lang plai-typed

(require "python-core-syntax.rkt")

#|

Here is a suggestion for how to implement shared runtime functionality -
write it as core expression forms and use python-lib to wrap your
desugared expressions in an environment that will contain useful
bindings.  For example, this sample library binds `print` to a function
that calls the primitive `print`.

|#

(define-type-alias Lib (CExp -> CExp))

(define print-lambda
  (CFunc (list 'to-print) (hash (list))
    (CPrim1 'print (CId 'to-print))))

(define assert-true-lambda
  (CFunc (list 'check-true) (hash (list))
    (CIf (CId 'check-true) (CId 'True) (CRaise (CApp (CId 'Exception) (list (CStr "Assert failed")))))))

(define assert-false-lambda
  (CFunc (list 'check-false) (hash (list))
    (CIf (CId 'check-false) (CRaise (CApp (CId 'Exception) (list (CStr "Assert failed")))) (CId 'True))))

(define fail-lambda
  (CFunc (list 'msg) (hash (list))
    (CRaise (CId 'msg))))

(define-type LibBinding
  [bindO (left : symbol) (right : CObject)]
  [bindE (left : symbol) (right : CExp)])

(define python-library
  (list (bindO 'True py-True)
        (bindO 'False py-False)
        (bindO 'None py-None)
        (bindO 'NotImplemented py-NotImplemented)
        (bindO 'Exception py-Exception)
        (bindO 'NameError py-NameError)
        (bindO 'BaseException py-BaseException)
        (bindO 'TypeError py-TypeError)
        (bindO 'UnboundLocalError py-UnboundLocalError)
        (bindE 'print print-lambda)
        (bindE '___assertTrue assert-true-lambda)
        (bindE '___assertFalse assert-false-lambda)
;        (bindE '___assertIn asset-in-lambda)
;        (bindE '___assertNotIn assert-not-in-lambda)
;        (bindE '___assertEqual asset-equal-lambda)
;        (bindE '___assertNotEqual assert-not-equal-lambda)
;        (bindE '___assertRaises assert-raises-lambda)
;        (bindE '___assertIs assert-is-lambda)
        (bindE '___fail fail-lambda)
        ))

(define (get-library [id : symbol] [lv : (listof LibBinding)])
  (cond
    [(empty? lv)
     (error 'get-library "not found")]
    [(symbol=? id (type-case LibBinding (first lv)
                    (bindO (l r) l)
                    (bindE (l r) l)))
     (first lv)]
    [else (get-library id (rest lv))]))

(define (get-library-names [lv : (listof LibBinding)]) : (listof symbol)
  (cond
    [(empty? lv) empty]
    [else (cons (type-case LibBinding (first lv)
                  (bindO (l r) l)
                  (bindE (l r) l))
                (get-library-names (rest lv)))]))


(define (python-lib expr)
  (local [(define (python-lib/recur libs)
            (cond [(empty? libs) expr]
                  [(cons? libs)
                   (type-case LibBinding (first libs)
                     [bindE (name value)
                            (CSeq (CSetExp name value)
                                  (python-lib/recur (rest libs)))]
                     [bindO (name value)
                            (CSeq (CSetObj name value)
                                  (python-lib/recur (rest libs)))])]))]
    (python-lib/recur python-library)))
