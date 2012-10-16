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

(define (run-python port python-path)
  (interp
    (python-lib
      (desugar
        (get-structured-python
          (parse-python/port port python-path))))))

(define python-path "/home/joe/bin/python")

(command-line
  #:once-each
  ("--interp" "Interpret stdin as python"
   (run-python (current-input-port) python-path))

  ("--get-syntax" "Get s-exp for python"
   (pretty-write (parse-python/port (current-input-port) python-path)))

  ("--test" dirname "Run all tests in dirname"
   (display (results-summary (run-tests (mk-proc-eval/silent run-python) dirname))))

  ("--python-path" path "Set the python path"
   (set! python-path path))

)
