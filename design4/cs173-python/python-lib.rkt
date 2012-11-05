#lang plai-typed

(require "python-core-syntax.rkt")

(define-type-alias Lib (CExp -> CExp))

(define print-lambda
  (CFn (list 'to-print)
    (CPrim 'print (list (CId 'to-print)))))

(define assert-true-lambda
  (CFn (list 'check-true)
    (CIf (CId 'check-true) (CBool true) (CRaise (CStr "Assert failed")))))

(define assert-false-lambda
  (CFn (list 'check-false)
    (CIf (CId 'check-false) (CBool false) (CRaise (CStr "Assert failed")))))

(define assert-equal-lambda
  (CFn (list 'arg1 'arg2)
    (CIf (CPrim 'equal (list (CId 'arg1) (CId 'arg2))) (CBool true) (CRaise (CStr "Assert failed")))))

(define assert-not-equal-lambda
  (CFn (list 'arg1 'arg2)
    (CIf (CApp (CId '^not) (list (CPrim 'equal (list (CId 'arg1) (CId 'arg2)))))
         (CBool true)
         (CRaise (CStr "Assert failed")))))

(define and-lambda
  (CFn (list 'arg1 'arg2)
    (CIf (CPrim '^truthy (list (CId 'arg1)))
      (CIf (CPrim '^truthy (list (CId 'arg2)))
        (CBool true)
        (CBool false))
      (CBool false))))

(define or-lambda
  (CFn (list 'arg1 'arg2)
    (CIf (CPrim '^truthy (list (CId 'arg1)))
      (CBool true)
      (CIf (CPrim '^truthy (list (CId 'arg2)))
          (CBool true)
          (CBool false)))))

(define not-lambda
  (CFn (list 'arg)
    (CIf (CPrim '^truthy (list (CId 'arg)))
      (CBool false)
      (CBool true))))


(define true-val
  (CBool true))
(define false-val
  (CBool false))

(define-type LibBinding
  [bind (left : symbol) (right : CExp)])

(define lib-functions
  (list (bind 'print print-lambda)
        (bind 'True true-val)
        (bind 'False true-val)
        (bind '^and and-lambda)
        (bind '^or or-lambda)
        (bind '^not not-lambda)
        (bind '___assertTrue assert-true-lambda)
        (bind '___assertFalse assert-false-lambda)
        (bind '___assertEqual assert-equal-lambda)
        (bind '___assertNotEqual assert-not-equal-lambda)
;        (bind '___assertIn assert-in-lambda)
;        (bind '___assertNotIn assert-not-in-lambda)
;        (bind '___assertRaises assert-raises-lambda)
;        (bind '___assertIs assert-is-lambda)
;        (bind '___fail fail-lambda)
))

(define (python-lib expr)
  (local [(define (python-lib/recur libs)
            (cond [(empty? libs) expr]
                  [(cons? libs)
                   (type-case LibBinding (first libs)
                     (bind (name value)
                           (CLet name value
                                 (python-lib/recur (rest libs)))))]))]
    (python-lib/recur lib-functions)))


