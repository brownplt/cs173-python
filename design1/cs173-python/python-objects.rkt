#lang plai-typed

(require "python-core-syntax.rkt")

(define (obj-type)
  (CObject (VStr "none") 
           (VNone) 
           (make-hash (list (values "%setattr"
                                    (CFunc (list 'self 'id 'val) (CSetField 
                                                                  (CId 'self) 
                                                                  (CId 'id)
                                                                  (CId'val))))
                            (values "%getattr"
                                    (CFunc (list 'self 'id) (CGetField 
                                                             (CId 'self) 
                                                             (CId 'id))))
                            (values "%eq"
                                    (CFunc (list 'self 'id)
                                           (CId 'False)))
                            (values "%ne"
                                    (CFunc (list 'self 'id)
                                           (CId 'False)))
                            (values "%gt"
                                    (CFunc (list 'self 'id)
                                           (CId 'False)))
                            (values "%ge"
                                    (CFunc (list 'self 'id)
                                           (CId 'False)))
                            (values "%lt"
                                    (CFunc (list 'self 'id)
                                           (CId 'False)))
                            (values "%le"
                                    (CFunc (list 'self 'id)
                                           (CId 'False)))
                            (values "%is"
                                    (CFunc (list 'self 'id)
                                           (CId 'False)))                   
                            ))))
#|
(define (exn-type message)
  (CObject (make-hash (list (values "%class"
                                    (obj-type))
                            (values "%primval"
                                    message)
                            (values "%str"
                                    (CFunc empty message))
                            
))))
|#

(define (none-type)
  (CObject (VStr "none")
           (VNone) 
           (make-hash (list (values "%class"
                                    (obj-type))
                            (values "%str"
                                    (CFunc (list 'self)
                                           (%to-object (VStr "None"))))
                            
                            ))))

(define (str-type primval)
  (CObject (VStr "string") 
           primval 
           (make-hash (list (values "%class"
                                    (obj-type))
                            (values "%len"
                                    (CFunc (list 'self)
                                           (CPrim1 'len
                                                   (CId 'self))))
                            (values "%eq"
                                    (CFunc (list 'self 'right)
                                           (CPrim2 'seq
                                                   (CId 'self)
                                                   (CId 'right))))
                            (values "%ne"
                                    (CFunc (list 'self 'right)
                                           (CPrim2 'sne
                                                   (CId 'self)
                                                   (CId 'right))))
                            (values "%gt"
                                    (CFunc (list 'self 'right)
                                           (CPrim2 'sgt
                                                   (CId 'self)
                                                   (CId 'right))))
                            (values "%ge"
                                    (CFunc (list 'self 'right)
                                           (CPrim2 'sge
                                                   (CId 'self)
                                                   (CId 'right))))
                            (values "%lt"
                                    (CFunc (list 'self 'right)
                                           (CPrim2 'slt
                                                   (CId 'self)
                                                   (CId 'right))))
                            (values "%le"
                                    (CFunc (list 'self 'right)
                                           (CPrim2 'sle
                                                   (CId 'self)
                                                   (CId 'right))))
                            (values "%str"
                                    (CFunc (list 'self) (CId 'self)))
                            
                            ))))

(define (int-type primval)
  (CObject (VStr "int")
           primval 
           (make-hash (list (values "%class"
                                    (obj-type))
                            (values "%add" ;;assumes right side is an object
                                    (CFunc (list 'self 'right)
                                           (CPrim2 'numplus
                                                   (CId 'self)
                                                   (CId 'right))))
                            (values "%sub" ;;assumes right side is an object
                                    (CFunc (list 'self 'right)
                                           (CPrim2 'numsub
                                                   (CId 'self)
                                                   (CId 'right))))
                            (values "%mul" ;;assumes right side is an object
                                    (CFunc (list 'self 'right)
                                           (CPrim2 'nummul
                                                   (CId 'self)
                                                   (CId 'right))))
                            (values "%div" ;;assumes right side is an object
                                    (CFunc (list 'self 'right)
                                           (CPrim2 'numdiv
                                                   (CId 'self)
                                                   (CId 'right))))
                            (values "%floordiv" ;;assumes right side is an object
                                    (CFunc (list 'self 'right)
                                           (CPrim2 'numfloordiv
                                                   (CId 'self)
                                                   (CId 'right))))
                            (values "%mod" ;;assumes right side is an object
                                    (CFunc (list 'self 'right)
                                           (CPrim2 'nummod
                                                   (CId 'self)
                                                   (CId 'right))))
                            (values "%or" ;;assumes right side is an object
                                    (CFunc (list 'self 'right)
                                           (CPrim2 'or
                                                   (CId 'self)
                                                   (CId 'right))))
                            (values "%and" ;;assumes right side is an object
                                    (CFunc (list 'self 'right)
                                           (CPrim2 'and
                                                   (CId 'self)
                                                   (CId 'right))))
                            (values "%le" ;;assumes right side is an object
                                    (CFunc (list 'self 'right)
                                           (CPrim2 'le
                                                   (CId 'self)
                                                   (CId 'right))))
                            (values "%lt" ;;assumes right side is an object
                                    (CFunc (list 'self 'right)
                                           (CPrim2 'lt
                                                   (CId 'self)
                                                   (CId 'right))))
                            (values "%ge" ;;assumes right side is an object
                                    (CFunc (list 'self 'right)
                                           (CPrim2 'ge
                                                   (CId 'self)
                                                   (CId 'right))))
                            (values "%gt" ;;assumes right side is an object
                                    (CFunc (list 'self 'right)
                                           (CPrim2 'gt
                                                   (CId 'self)
                                                   (CId 'right))))
                            (values "%eq" ;;assumes right side is an object
                                    (CFunc (list 'self 'right)
                                           (CPrim2 'eq
                                                   (CId 'self)
                                                   (CId 'right))))
                            (values "%is" ;;assumes right side is an object
                                    (CFunc (list 'self 'right)
                                           (CPrim2 'eq
                                                   (CId 'self)
                                                   (CId 'right))))
                            (values "%ne" ;;assumes right side is an object
                                    (CFunc (list 'self 'right)
                                           (CPrim2 'ne
                                                   (CId 'self)
                                                   (CId 'right))))
                            (values "%not"
                                    (CFunc (list 'self)
                                           (CIf (CId 'self)
                                                (CId 'False)
                                                (CId 'True))))
                            (values "%str"
                                    (CFunc (list 'self)
                                           (%to-object (VStr (type-case PrimVal primval
                                                               [VNum (n) (to-string n)]
                                                               [else "42"])))))
                            
                            ))))

(define (bool-type primval)
  (CObject (VStr "bool")
           primval 
           (make-hash (list (values "%class"
                                    (int-type primval))
                            (values "%str"
                                    (CFunc (list 'self)
                                           (%to-object (VStr
                                                        (type-case PrimVal primval
                                                          [VNum (n) (cond
                                                                      [(= 0 n) "False"]
                                                                      [else "True"])]
                                                          [else "True"])))))
                            ))))

(define (%to-object (val : PrimVal)) : CExp
  (type-case PrimVal val
    [VNum (n) (int-type val)]
    [VTrue () (bool-type (VNum 1))]
    [VFalse () (bool-type (VNum 0))]
    [VStr (s) (str-type val)]
    ;;[CError (exn) (exn-type exn)]
    [else (error 'to-object "not implemented object yet")]))