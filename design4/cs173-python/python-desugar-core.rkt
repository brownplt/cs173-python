#lang plai-typed

;; this is where the core is desugared into the micro-core.

(require "python-core-syntax.rkt"
         "python-micro-syntax.rkt"
         "python-helpers.rkt")

(define (desugar-core [e : CExp]) : UExp
  (type-case CExp e
    [CNum (n) (UNum n)]
    [CStr (s) (UStr s)]
    [CBool (b) (UBool b)]
    [CDict (fs) (UMap (hash-map fs (lambda (k v) (pair (desugar-core k) (desugar-core v)))))]
    [CObj (fs) (UMap (hash (map (lambda (f) (pair (UId (fieldC-n f)) (desugar-core (fieldC-v f)))) fs)))]
    [CList (es) (UList (map desugar-core es))]
    [CNone () (UNone)]

    [CSeq (e1 e2) (USeq (desugar-core e1) (desugar-core e2))]
    [CIf (test then els) (UIf (desugar-core test) (desugar-core then) (desugar-core els))]
    [CPass () (UNone)]

    [CId (x) (UId x)]
    [CLet (x bind body) (ULet x (desugar-core bind) (desugar-core body))]
    [CSet (x v) (USet x (desugar-core v))]

    [CApp (fun args) (if (= 0 (length args))
                         (UApp (desugar-core fun) (UNone))
                         (foldr (lambda (arg exp) (UApp exp arg)) 
                                (desugar-core fun) 
                                (map desugar-core args)))]

    [CPrim (op args) (UPrim op (map desugar-core args))]

    ;; these are the things we are actually changing

    ;; basically, we provide a shadowed '^throw continuation, and then see what
    ;; we are handed. if it is an error (ie, an exception), we see if it matches
    ;; what we are catching, and if it doesn't, we re-throw. Since we are out of
    ;; scope of the current try/catch, we will now pick up whatever exception
    ;; continuation is outside. note that this obviously requires the entire program
    ;; to be wrapped in a try/catch that matches on everything.
    [CTry (bdy mat cat)
      (ULet '^rv
        (ULetCC '^throw
          (desugar-core bdy))
        (UIf (UPrim 'equal (list (UPrim 'ty (list (UId '^rv))) (UStr "error")))
          (UIf (UApp (desugar-core mat) (UPrim 'err-v (list (UId '^rv))))
              (desugar-core cat)
              (UApp (UId '^throw) (UId '^rv)))
          (UId '^rv)))]
    [CRaise (e) (UApp (UId '^throw) (desugar-core e))]

    ;; while loops desugar into recursion, with the added break and continue continuations
    [CWhile (cnd bdy)
      (ULetCC '^break
        (ULet '^loop (UNone)
          (USeq (USet '^loop
            (UFn '_
              (UIf (desugar-core cnd)
                   (USeq (ULetCC '^continue
                            (desugar-core bdy))
                         (UApp (UId '^loop) (UNone)))
                    (UNone))))
                (UApp (UId '^loop) (UNone)))))]
    [CBreak () (UApp (UId '^break) (UNone))]
    [CContinue () (UApp (UId '^continue) (UNone))]


    ;; early return is handled by binding a return continuation inside functions
    [CFn (args body)
      (ULetCC '^ret
              (if (= 0 (length args))
                  (UFn '^_ (desugar-core body))
                  (foldr (lambda (arg exp) (UFn arg exp))
                         (desugar-core body)
                         args)))]
    [CRet (v) (UFn '^arg (UApp (UId '^ret) (UId '^arg)))]

    [CUndefined () (UUndefined)]))

(test (desugar-core (CFn (list 'a 'b 'c) (CNone))) (ULetCC '^ret (UFn 'a (UFn 'b (UFn 'c (UNone))))))
(test (desugar-core (CApp (CFn (list 'a 'b) (CNone)) (list (CNum 1) (CNum 2))))
      (UApp (UApp (ULetCC '^ret (UFn 'a (UFn 'b (UNone)))) (UNum 2)) (UNum 1)))