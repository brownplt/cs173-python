#lang plai

(require racket/match
         racket/list)

(define (pretty-struct s)
  (pretty-write s))

(define (pretty-store s)
  (pretty-write s))

(define (pretty-scopedb s)
  (pretty-write s))