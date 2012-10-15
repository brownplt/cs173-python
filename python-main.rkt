#lang racket/base

(require racket/cmdline
         racket/pretty
         "parse-python.rkt"
         "get-structured-python.rkt"
         "python-interp.rkt"
         "python-desugar.rkt"
         "python-lib.rkt")

(command-line
  #:once-each
  ("--interp" "Interpret stdin as python"
   (interp
     (python-lib
       (desugar
         (get-structured-python
           (parse-python/port "stdin" (current-input-port)))))))
  ("--get-syntax" "Get s-exp for python"
   (pretty-write (parse-python/port "stdin" (current-input-port))))
)
