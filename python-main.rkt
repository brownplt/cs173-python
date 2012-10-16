#lang racket/base

(require racket/cmdline
         racket/pretty
         "parse-python.rkt"
         "get-structured-python.rkt"
         "python-interp.rkt"
         "python-desugar.rkt"
         "python-lib.rkt"
         "run-tests.rkt"
         "python-evaluator.rkt")

(define (run-python name port)
  (interp
    (python-lib
      (desugar
        (get-structured-python
          (parse-python/port name port))))))

(command-line
  #:once-each
  ("--interp" "Interpret stdin as python"
   (run-python "stdin" (current-input-port)))

  ("--get-syntax" "Get s-exp for python"
   (pretty-write (parse-python/port "stdin" (current-input-port))))

  ("--test" dirname "Run all tests in dirname"
   (display (results-summary (run-tests (mk-proc-eval/silent run-python) dirname))))

)
