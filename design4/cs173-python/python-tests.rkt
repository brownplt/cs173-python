#lang plai-typed

;; this module exercises the whole stack on some toy examples (no python-lib)

(require "python-interp.rkt"
         "python-desugar.rkt"
         "python-desugar-core.rkt"
         "python-cps.rkt"
         "python-syntax.rkt"
         "python-micro-syntax.rkt"
         )


(define (run [e : PyExp]) : UVal
    (interp (run-cps (desugar-core (desugar e)))))

(test (run (PyNum 10)) (VNum 10))
(test (run (PyStr "s")) (VStr "s"))
(test (run (PyIf (PyBool true) (PyNum 1) (PyNum 2))) (VNum 1))
(test (run (PyWhile (PyBool true) (PyIf (PyBool true) (PyBreak) (PyContinue)))) (VNone))
