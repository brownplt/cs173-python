#lang plai-typed

(require "python-micro-syntax.rkt"
         "python-primitives.rkt"
         "python-helpers.rkt"
         "python-cps.rkt")

(define counter : (-> Location)
  (local [(define n 0)]
    (lambda () (begin (set! n (+ n 1)) n))))

(define (interp-env [expr : UExp] [env : Env] [st : Store]) : VStateT
  (local [
    ;; debugging output
    ;(define _1 (display (string-append (to-string expr) "\n\n")))
    ;(define _2 (display (string-append (to-string st) "\n\n")))
    ;(define _3 (display (string-append (to-string env) "\n\n")))
          
    ;; the id-state, means nothing is changed.
    (define (ids [v : UVal]) : VStateT (VState v st env))]
  (type-case UExp expr
    [UNum (n) (ids (VNum n))]
    [UStr (s) (ids (VStr s))]
    [UBool (b) (ids (VBool b))]
    [UNone () (ids (VNone))]
    [UMap (fs) (let [(new-fs (make-hash empty))]
      (VState (VMap new-fs) (snd (foldl (lambda (e bs)
        (letrec [(kv (interp-env e env (snd bs)))
                 (vv (interp-env (some-v (hash-ref fs e)) env (VState-s kv)))]
          (pair (hash-set! new-fs (VState-v kv) (VState-v vv)) (VState-s vv))))
        (pair (void) st) (hash-keys fs))) env))]

    [UError (e) (ids (VError (VState-v (interp-env e env st))))]

    [UIf (i t e) (let [(is (interp-env i env st))]
                    (if (truthy (VState-v is))
                        (interp-env t (VState-e is) (VState-s is))
                        (interp-env e (VState-e is) (VState-s is))))]

    [UId (x) (type-case (optionof Location) (hash-ref env x)
      [some (l) (type-case (optionof UVal) (hash-ref st l)
        [some (v) (ids v)]
        [none () (error 'interp (string-append "Store error: "
                                              (symbol->string x)))])]
      [none () (error 'interp (string-append "Unbound identifier: "
                                             (symbol->string x)))])]
    [ULet (x bind body)
          (let [(bs (interp-env bind env st))
                (loc (counter))]
            (interp-env body (hash-set (VState-e bs) x loc)
                             (hash-set (VState-s bs) loc (VState-v bs))))]
    [USet (x bind)
          (let [(bs (interp-env bind env st))]
            (type-case (optionof Location) (hash-ref env x)
              [some (l)
                (VState (VState-v bs) (hash-set (VState-s bs) l (VState-v bs)) (VState-e bs))]
              [none ()
                    (let [(loc (counter))]
                        (VState (VState-v bs)
                                (hash-set (VState-s bs) loc (VState-v bs))
                                (hash-set (VState-e bs) x loc)))]))]
    [USeq (e1 e2)
      (letrec [(v1 (interp-env e1 env st))
               (dbg (display (to-string v1)))]
        (interp-env e2 (VState-e v1) (VState-s v1)))]

    [UApp (fun arge)
          (let [(fs (interp-env fun env st))]
     (type-case UVal (VState-v fs)
       [VClosure (cenv argx body)
         (letrec [(argv (interp-env arge env (VState-s fs)))
                  (es (hash-set-pair (pair cenv (VState-s argv)) (counter) argx (VState-v argv)))
                  (res (interp-env body (fst es) (snd es)))]
           (VState (VState-v res) (VState-s res) (VState-e argv)))]
       [else (error 'interp "Not a closure")]))]

    [UFn (arg body) (begin
      (ids (VClosure env arg body)))]

    [UPrim (op args) (let [(argvs (foldr (lambda (a bs)
                                  (let [(vs (interp-env a env (snd bs)))]
                                    (pair (cons (VState-v vs) (fst bs)) (VState-s vs))))
                                (pair empty st)
                                args))]
      (VState (prim op (fst argvs)) (snd argvs) env))]
    [UUndefined () (error 'undefined "trying to evaluate UUndefined")]
    [else (error 'interp (string-append "haven't defined interp for this yet: " (to-string expr)))])))

(define (hash-set-pair [es : (Env * Store)] [n : Location] [k : symbol] [v : UVal]) : (Env * Store)
  (values (hash-set (fst es) k n) (hash-set (snd es) n v)))

(define (interp expr)
  (VState-v (interp-env expr (hash (list)) (hash (list)))))


(test (interp (UNum 1)) (VNum 1))
(test (interp (UStr "hello")) (VStr "hello"))
(test (interp (UBool true)) (VBool true))
(test (interp (UApp (UFn 'a (UId 'a)) (UNum 1))) (VNum 1))
(test (interp (UApp (UFn 'f (UApp (UId 'f) (UNum 1))) (UFn 'a (UPrim '+ (list (UNum 1) (UId 'a))))))
      (VNum 2))
(test (interp (UNone)) (VNone))
(test (interp (UPrim '+ (list (UNum 1) (UNum 1)))) (VNum 2))
(test (interp (UPrim 'str+ (list (UStr "hi") (UStr "hi")))) (VStr "hihi"))
(test (interp (UPrim '/ (list (UNum 1) (UNum 1)))) (VNum 1))
(test (interp (UPrim '* (list (UNum 2) (UNum 1)))) (VNum 2))
(test (interp (UPrim '- (list (UNum 1) (UNum 1)))) (VNum 0))
(test (interp (UPrim '< (list (UNum 1) (UNum 2)))) (VBool true))
(test (interp (UPrim '< (list (UNum 3) (UNum 2)))) (VBool false))
(test (interp (UPrim '< (list (UNum 2) (UNum 2)))) (VBool false))
(test (interp (UPrim '<= (list (UNum 1) (UNum 2)))) (VBool true))
(test (interp (UPrim '<= (list (UNum 3) (UNum 2)))) (VBool false))
(test (interp (UPrim '<= (list (UNum 2) (UNum 2)))) (VBool true))
(test (interp (UPrim '> (list (UNum 1) (UNum 2)))) (VBool false))
(test (interp (UPrim '> (list (UNum 3) (UNum 2)))) (VBool true))
(test (interp (UPrim '> (list (UNum 2) (UNum 2)))) (VBool false))
(test (interp (UPrim '>= (list (UNum 1) (UNum 2)))) (VBool false))
(test (interp (UPrim '>= (list (UNum 3) (UNum 2)))) (VBool true))
(test (interp (UPrim '>= (list (UNum 2) (UNum 2)))) (VBool true))
(test (interp (UPrim 'str< (list (UStr "a") (UStr "b")))) (VBool true))
(test (interp (UPrim 'str< (list (UStr "c") (UStr "b")))) (VBool false))
(test (interp (UPrim 'str< (list (UStr "b") (UStr "b")))) (VBool false))
(test (interp (UPrim 'str<= (list (UStr "a") (UStr "b")))) (VBool true))
(test (interp (UPrim 'str<= (list (UStr "c") (UStr "b")))) (VBool false))
(test (interp (UPrim 'str<= (list (UStr "b") (UStr "b")))) (VBool true))
(test (interp (UPrim 'str> (list (UStr "a") (UStr "b")))) (VBool false))
(test (interp (UPrim 'str> (list (UStr "c") (UStr "b")))) (VBool true))
(test (interp (UPrim 'str> (list (UStr "b") (UStr "b")))) (VBool false))
(test (interp (UPrim 'str>= (list (UStr "a") (UStr "b")))) (VBool false))
(test (interp (UPrim 'str>= (list (UStr "c") (UStr "b")))) (VBool true))
(test (interp (UPrim 'str>= (list (UStr "b") (UStr "b")))) (VBool true))

;; some examples with cps transformation
(test (interp (run-cps (ULetCC '^jmp (USeq (UApp (UId '^jmp) (UNum 1))
                                           (UNum 2)))))
      (VNum 1))

(test (interp (run-cps (ULet 'n (UNone)
                             (USeq (USet 'n (UNum 1))
                                   (UId 'n)))))
      (VNum 1))

(test (interp (run-cps (UNum 1))) (VNum 1))
(test (interp (run-cps (UIf (UBool true) (UNum 1) (UNum 2)))) (VNum 1))
(test (interp (run-cps (UApp (UFn 'a (UId 'a)) (UNum 1)))) (VNum 1))

(test (interp
       (run-cps
        (ULetCC '^break
                (ULet '^loop (UNone)
                      (USeq
                       (USet '^loop
                             (UFn '_
                                  (UIf (UBool true)
                                       (USeq (ULetCC '^continue
                                                     (UIf (UBool true)
                                                          (UApp (UId '^break) (UNone))
                                                          (UApp (UId '^continue) (UNone))))
                                             (UApp (UId '^loop) (UNone)))
                                                   (UNone))))
                       (UApp (UId '^loop) (UNone)))))))
      (VNone))



