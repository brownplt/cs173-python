#lang plai-typed

(require "python-core-syntax.rkt"
         "python-primitives.rkt"
         "python-objects.rkt")

(define (extract-prim (obj : CVal)) : PrimVal
  (type-case CVal obj
    [VObject (pval fields) pval]))

(define (booleanof (obj : CVal)) : boolean
  (type-case CVal obj
    [VObject (primval fields) (type-case PrimVal primval
                                [VNum (n) (not (= n 0))]
                                [else #t])]))

(define (boolean->primval (bool : boolean)) : PrimVal
  (if bool (VTrue) (VFalse)))

(define (Combine (op : symbol) (left : CVal) (right : CVal)) : CExp
  (let ([left-prim (extract-prim left)]
        [right-prim (extract-prim right)])
    (%to-object (cond
                  [(and (VNum? left-prim) (VNum? right-prim)) 
                   (cond
                     [(symbol=? op 'nummul) (VNum (* (VNum-n left-prim) 
                                                     (VNum-n right-prim)))]
                     [(symbol=? op 'numplus) (VNum (+ (VNum-n left-prim) 
                                                      (VNum-n right-prim)))]
                     [(symbol=? op 'numfloordiv) (VNum (floor (/ (VNum-n left-prim) 
                                                                 (VNum-n right-prim))))]
                     [(symbol=? op 'numdiv) (VNum (/ (VNum-n left-prim) 
                                                     (VNum-n right-prim)))]
                     [(symbol=? op 'nummod) (VNum (modulo (VNum-n left-prim) 
                                                          (VNum-n right-prim)))]
                     [(symbol=? op 'numsub) (VNum (- (VNum-n left-prim) 
                                                     (VNum-n right-prim)))]
                     [(symbol=? op 'and) (boolean->primval (and (booleanof left)
                                                                (booleanof right)))]
                     [(symbol=? op 'or) (boolean->primval (or (booleanof left)
                                                              (booleanof right)))]
                     [(symbol=? op 'gt) (boolean->primval (> (VNum-n left-prim)
                                                             (VNum-n right-prim)))]
                     [(symbol=? op 'ge) (boolean->primval (>= (VNum-n left-prim)
                                                              (VNum-n right-prim)))]
                     [(symbol=? op 'lt) (boolean->primval (< (VNum-n left-prim)
                                                             (VNum-n right-prim)))]
                     [(symbol=? op 'le) (boolean->primval (<= (VNum-n left-prim)
                                                              (VNum-n right-prim)))]
                     [(symbol=? op 'eq) (boolean->primval (= (VNum-n left-prim)
                                                             (VNum-n right-prim)))]
                     [(symbol=? op 'ne) (boolean->primval (not (= (VNum-n left-prim)
                                                             (VNum-n right-prim))))])]
                  
                  [(and (VStr? left-prim) (VStr? right-prim))
                   (cond
                     [(symbol=? op 'stringplus) (VStr (string-append (VStr-s left-prim) 
                                                                     (VStr-s right-prim)))])]
                  
                  [else (error 'Combine (foldl string-append "" (list "Couldn't handle "
                                                                      (to-string left-prim)
                                                                      " and "
                                                                      (to-string right-prim)
                                                                      " with "
                                                                      (symbol->string op))))]))))