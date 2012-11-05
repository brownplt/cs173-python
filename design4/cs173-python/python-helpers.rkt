#lang plai-typed

;; This module contains code that is not python specific

(print-only-errors true)

;; helpful pretty printing support
(require (typed-in racket/pretty [pretty-format : ('a -> string)]))
(require (typed-in racket/pretty [pretty-print : ('a -> void)]))
(define (trace a b)
  (begin (pretty-print a)
         b))


;; hash-mut-copy: makes a mutable copy of a hashtable
(define (hash-mut-copy [o : (hashof 'a 'b)]) : (hashof 'a 'b)
  (local [(define h (make-hash (list)))]
    (begin
      (map (lambda (k) (hash-set! h k (some-v (hash-ref o k)))) (hash-keys o))
      h)))

;; pair takes a pair of values and creates a 2-tuple. this is needed
;; because values cannot be passed to higher order functions.
(define (pair [a : 'a] [b : 'b]) : ('a * 'b)
  (values a b))

(test (pair 1 2) (values 1 2))
(test (pair "hello" 100) (values "hello" 100))

;; fst returns the first of a 2-tuple
(define (fst [x : ('a * 'b)]) : 'a
  (local ([define-values (a b) x])
    a))
(test (fst (pair 1 2)) 1)
(test (fst (pair 'abc 2)) 'abc)

;; snd returns the second of a 2-tuple
(define (snd [x : ('a * 'b)]) : 'a
  (local ([define-values (a b) x])
    b))
(test (snd (pair 1 2)) 2)

;; zip takes two lists and a function that takes an element from each.
;; it creates the result of combining the two lists.
;; note that the behavior is not defined in the case that the lists are
;; note of equal length (ie, any behavior is an implementation detail and
;; subject to change).
(define (zip [f : ('a 'b -> 'c)]
             [al : (listof 'a)]
             [bl : (listof 'b)]) : (listof 'c)
  (cond
    [(empty? al) empty]
    [else (cons (f (first al) (first bl)) (zip f (rest al) (rest bl)))]))

(test (zip (lambda (a b) (values a b)) (list 1 2 3) (list 4 5 6))
    (list (values 1 4) (values 2 5) (values 3 6)))
(test (zip + (list 1 2 3) (list 4 5 6))
    (list 5 7 9))

;; init returns all of a list except for the last element. it errors on an empty
;; list
(define (init [l : (listof 'a)]) : (listof 'a)
  (reverse (rest (reverse l))))

(test (init (list 1 2 3)) (list 1 2))
(test (init (list 1)) (list))

;; hash-map maps over key, value pairs, creating a new hashtable with the results
(define (hash-map [h : (hashof 'a 'b)] [f : ('a 'b -> ('c * 'd))]) : (hashof 'c 'd)
    (let ([ks (hash-keys h)] [nh (make-hash (list))])
      (begin
        (map (lambda (k)
            (local ([define-values (new-k new-v) (f k (some-v (hash-ref h k)))])
              (hash-set! nh new-k new-v)))
             ks)
        nh)))

(test (hash-keys (hash-map (hash (list (values 1 2)))
                (lambda (a b) (values (+ 1 a) (- 2 b)))))
      (list 2))
(test (hash-ref (hash-map (hash (list (values 1 2)))
                  (lambda (a b) (values (+ 1 a) (- 2 b))))
                2)
      (some 0))


;; build-list takes a function and an integer and applys the function
;; to all integers from 0 up to the integer (non-inclusive) and returns
;; the list of resulting values.
(define (build-list [n : number] [f : (number -> 'a)]) : (listof 'a)
  (local [(define (bl-int c n f)
            (cond
              [(< c n) (cons (f c) (bl-int (+ c 1) n f))]
              [else empty]))]
    (bl-int 0 n f)))

(test (build-list 5 (lambda (x) x)) (list 0 1 2 3 4))
(test (build-list 5 (lambda (x) (* x x))) (list 0 1 4 9 16))