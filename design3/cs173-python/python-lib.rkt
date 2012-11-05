#lang plai-typed

(require "python-core-syntax.rkt")

#|

Here is a suggestion for how to implement shared runtime functionality -
write it as core expression forms and use python-lib to wrap your
desugared expressions in an environment that will contain useful
bindings.  For example, this sample library binds `print` to a function
that calls the primitive `print`.

|#

(define assert-true
  (CFunc (list 'f)
         (none)
         (CIf (CId 'f)
              (CNone)
              (CError (CStr "assertion failed")))))
(define assert-equal
  (CFunc (list 'a 'b)
         (none)
         (CIf (CApp (CPrimF 'equal)
                    (list (CId 'a)
                          (CId 'b))
                    (CTuple empty))
              (CNone)
              (CError (CStr "assertion failed")))))

(define partial-apply
  (CAddGlobal
   'partial-apply
   (CFunc (list 'func)
          (some 'args)
          (CReturn
           (CFunc empty
                  (some 'more-args)
                  (CReturn
                   (CApp (CId 'func)
                         empty
                         (CApp (CPrimF 'tuple-append)
                               (list (CId 'args) (CId 'more-args))
                               (CTuple empty)))))))))

(define class-type
  (CAddGlobal
   'class-type
   (CLet 'class-box (CBox (CUndefined))
         (CApp (CPrimF 'set-box)
               (list (CId 'class-box)
                     (CObj (CId 'class-box)
                           (CBox (CPrimMap (list)))))
               (CTuple empty)))))

(define-syntax-rule (make-type id fs)
  (define id
    (CAddGlobal
     'id
     (CObj (CBox (CPrimMap (map (lambda (pair)
                                  (local [(define-values (name val) pair)]
                                         (values (CStr name) val))) fs)))
           (CBox (CId 'type))))))

(make-type none-type
           (list (values "__bool__"
                         (CFunc (list 'this)
                                (none)
                                (CReturn (CFalse))))))

(make-type bool-type
           (list (values "__bool__"
                         (CFunc (list 'this)
                                (none)
                                (CReturn (CId 'this))))))

(make-type num-type
           (list (values "__bool__"
                         (CFunc (list 'this)
                                (none)
                                (CIf (CApp (CPrimF 'equal)
                                           (list (CId 'this)
                                                 (CNum 0))
                                           (CTuple empty))
                                     (CReturn (CFalse))
                                     (CReturn (CTrue)))))
                 (values "__add__"
                         (CPrimF 'int-add))
                 (values "__sub__"
                         (CPrimF 'int-sub))
                 (values "__neg__"
                         (CPrimF 'int-neg))))

(make-type str-type
           (list))

(make-type func-type
           (list))

(make-type obj-type
           (list (values "__bool__"
                         (CFunc (list 'this)
                                (none)
                                (CReturn
                                 (CIf (CApp (CPrimF 'class-has-member?)
                                            (list (CId 'this)
                                                  (CStr "__len__"))
                                            (CTuple empty))
                                      (CIf (CApp (CPrimF 'equal)
                                                 (list (CApp (CApp (CPrimF 'class-lookup)
                                                                   (list (CId 'this)
                                                                         (CStr "__len__"))
                                                                   (CTuple empty))
                                                             empty
                                                             (CTuple empty))
                                                       (CNum 0))
                                                 (CTuple empty))
                                           (CFalse)
                                           (CTrue))
                                      (CTrue)))))))

(make-type tuple-type
           (list (values "__len__"
                         (CPrimF 'tuple-length))
                 (values "__add__"
                         (CPrimF 'tuple-append))))

(define lib-binds
  (list
   (values 'print (CPrimF 'print))
   (values 'True (CTrue))
   (values 'False (CFalse))
   (values 'None (CNone))
   (values '___assertTrue assert-true)
   (values '___assertEqual assert-equal)
   (values 'type class-type)
   (values 'bool bool-type)
   (values 'int num-type)
   (values 'string str-type)
   (values 'tuple tuple-type)
   (values 'object obj-type)))

(define lib-exprs
  (list
   partial-apply
   none-type
   func-type))

(define (python-lib expr)
  (foldr (lambda (pair expr)
           (local [(define-values (name value) pair)]
                  (CLet name value
                        expr)))
         (foldr CSeq
                expr
                lib-exprs)
         lib-binds))
