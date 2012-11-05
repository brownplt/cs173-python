#lang plai-typed

(require "python-micro-syntax.rkt")

(define (run-cps [e : UExp])
  (UApp (cps e) (UFn '^x (UId '^x))))


;; NOTE - still need to handle primitives. most likely by converting to UPrim1 and UPrim2

(define (cps [e : UExp])
  (type-case UExp e
  [USeq (e1 e2)
    (UFn '^k
         (UApp (cps e1) (UFn '^_
                             (UApp (cps e2) (UId '^k)))))]

  [USet (s e)
    (UFn '^k
         (UApp (cps e) (UFn '^ev
                            (UApp (UId '^k) (USet s (UId '^ev))))))]

  [ULet (s e b)
        (cps (UApp (UFn s b) e))]

  [UError (e)
          (UFn '^k
               (UApp (cps e)
                     (UFn '^ev
                          (UApp (UId '^k) (UError (UId '^ev))))))]

  [UIf (tst thn els)
    (UFn '^k
         (UApp (cps tst)
               (UFn '^tstv
                    (UIf (UId '^tstv)
                         (UApp (cps thn) (UId '^k))
                         (UApp (cps els) (UId '^k))))))]

  [UApp (f a)
    (UFn '^k
         (UApp (cps f)
               (UFn '^fv
                    (UApp (cps a)
                          (UFn '^av
                               (UApp (UApp (UId '^fv) (UId '^av)) (UId '^k)))))))]

  [UFn (arg body)
    (UFn '^k
         (UApp (UId '^k)
               (UFn arg
                    (UFn '^dyn-k
                         (UApp (cps body) (UId '^dyn-k))))))]


  [ULetCC (sym body)
   (UFn '^k
        (ULet sym (UFn '^v
                       (UFn '^dyn-k
                            (UApp (UId '^k) (UId '^v))))
              (UApp (cps body) (UId '^k))))]

  ;; literal values
  [else
   (UFn '^k
        (UApp (UId '^k) e))]))

(test (cps (UNum 10)) (UFn '^k (UApp (UId '^k) (UNum 10))))
(test (cps (UStr "hi")) (UFn '^k (UApp (UId '^k) (UStr "hi"))))
(test (cps (UBool true)) (UFn '^k (UApp (UId '^k) (UBool true))))
(test (cps (UNone)) (UFn '^k (UApp (UId '^k) (UNone))))
