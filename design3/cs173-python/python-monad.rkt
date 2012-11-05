#lang plai-typed

(require "python-core-syntax.rkt")

(define-type (ROption 'a)
  [RValue (value : 'a)]
  [RReturn (value : CVal)]
  [RError (value : CVal)])

(define-type-alias (PM 'b)
  (Store -> (Store * (ROption 'b))))

(define (m-bind (m : (PM 'c)) (f : ('c -> (PM 'd)))) : (PM 'd)
  (lambda (store)
    (local [(define-values (n-store ret) (m store))]
           (type-case (ROption 'c) ret
             [RValue (v) ((f v) n-store)]
             [RReturn (v) (values n-store (RReturn v))]
             [RError (v) (values n-store (RError v))]))))

(define (m-return (v : 'e)) : (PM 'e)
  (lambda (store)
    (values store (RValue v))))

(define-syntax m-do
  (syntax-rules ()
    [(m-do ([id expr] more ...) body) ;;with body to return
     (m-bind expr
             (lambda (id)
               (m-do (more ...)
                     body)))]
    [(m-do ([expr] more ...) body)
     (m-bind expr
             (lambda (_)
               (m-do (more ...)
                     body)))]
    [(m-do [] body) (m-return body)]
    [(m-do ([expr])) ;;body-less version
     expr]
    [(m-do ([id expr] more ...))
     (m-bind expr
             (lambda (id)
               (m-do (more ...))))]
    [(m-do ([expr] more ...))
     (m-bind expr
             (lambda (_)
               (m-do (more ...))))]))

(define (pm-return (v : CVal))
  (lambda (store)
    (values store (RReturn v))))

(define (pm-error (v : CVal))
  (lambda (store)
    (values store (RError v))))

(define (pm-catch-return (m : (PM 'f)) (catch : (CVal -> (PM 'f)))) : (PM 'f)
  (lambda (store)
    (local [(define-values (n-store ret) (m store))]
           (type-case (ROption 'f) ret
             [RReturn (v) ((catch v) n-store)]
             [else (values n-store ret)]))))

(define (pm-catch-error (m : (PM 'f)) (catch : (CVal -> (PM 'f)))) : (PM 'f)
  (lambda (store)
    (local [(define-values (n-store ret) (m store))]
           (type-case (ROption 'f) ret
             [RError (v) ((catch v) n-store)]
             [else (values n-store ret)]))))

(define pm-get-store
  (lambda (store)
    (values store (RValue store))))

(define (pm-set-store (n-store : Store))
  (lambda (store)
    (values n-store (RValue n-store))))

(define (m-map (f : ('g -> (PM 'h))) (lst : (listof 'g))) : (PM (listof 'h))
  (if (empty? lst)
      (m-return empty)
      (m-do ([val (f (first lst))]
             [mapped-rest (m-map f (rest lst))])
            (cons val mapped-rest))))

(define (m-foldl (f : ('i 'j -> (PM 'j))) (start : (PM 'j)) (lst : (listof 'i))) : (PM 'j)
  (if (empty? lst)
      start
      (m-do ([start start]
             [(m-foldl f (f (first lst) start) (rest lst))]))))

(define (interp-error (s : string))
  (pm-error (VStr s)))

;;Special store locs:
;;-1 = next index to alloc
;;-2 = globals

(define empty-store
  (hash (list (values -1 (VNum 0))
              (values -2 (VPrimMap (hash empty))))))

(define (pm-lookup-store (l : Location)) : (PM CVal)
  (m-do ([store pm-get-store])
        (some-v (hash-ref store l))))

(define (pm-add-store (l : Location) (v : CVal)) : (PM CVal)
  (m-do ([store pm-get-store]
         [(pm-set-store (hash-set store l v))])
        v))

(define make-new-loc
  (m-do ([old (pm-lookup-store -1)]
         [loc (m-return (VNum-n old))]
         [(pm-add-store -1 (VNum (+ 1 loc)))])
        loc))

(define (add-new-loc (v : CVal)) : (PM Location)
  (m-do ([loc make-new-loc]
         [(pm-add-store loc v)])
        loc))
