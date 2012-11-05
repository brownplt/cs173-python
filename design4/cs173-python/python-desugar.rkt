#lang plai-typed

(print-only-errors true)

(require "python-syntax.rkt"
         "python-core-syntax.rkt"
         "python-helpers.rkt")

(define (desugar [expr : PyExp]) : CExp
  (type-case PyExp expr
    [PySeq (es) (foldl (lambda (e1 e2) (CSeq e2 (desugar e1))) (desugar (first es)) (rest es))]
    [PyNum (n) (CNum n)]
    [PyStr (s) (CStr s)]
    [PyBool (b) (CBool b)]
    [PyNone () (CNone)]
    [PyDict (fs) (CDict (hash-map fs
                            (lambda (k v) (values (desugar k) (desugar v)))))]
    [PyLambda (args body)
      (CFn (PyArgs-args args) (desugar body))]
    [PyFunDef (name args body)
      ;; ignoring default arguments for now.
      ;; also, just setting this in the current scope. is that correct?
      (CSeq (CSet name (CFn (PyArgs-args args) (desugar body)))
            (CId name))]
    [PyReturn (v) (CRet (desugar v))]
    [PyApp (f args) (CApp (desugar f) (map desugar args))]
    [PyId (x) (CId x)]
    [PyIf (tst then els) (CIf (desugar tst) (desugar then) (desugar els))]
    [PyPass () (CPass)]
    [PyBoolOp (op arg1 arg2)
      (case op
        ['And (CApp (CId '^and) (list (desugar arg1) (desugar arg2)))]
        ['Or  (CApp (CId '^or) (list (desugar arg1) (desugar arg2)))])]
    [PyBinOp (op left right)
      (case op
        ['Add (CPrim '+ (list (desugar left) (desugar right)))])]
    [PyUnaryOp (op arg)
      (case op
        ['Not (CApp (CId '^not) (list (desugar arg)))]
        ['USub (CPrim '- (list (CNum 0) (desugar arg)))])]
    [PyCompare (ops cmps lft)
      (desugar-compare ops cmps lft)]
    [PyAssign (targets value)
      ;; not doing tuple unpacking for now
      (CSet (PyId-x (first targets)) (desugar value))]
    [PyWhile (tst body) (CWhile (desugar tst) (desugar body))]
    [PyBreak () (CBreak)]
    [PyContinue () (CContinue)]
    [else (CUndefined)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Desugaring Specific Helpers ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; chain-lets: takes a list of (identifier, expr) pairs and evaluates the exprs
;; and assigns them the identifiers. Finally it takes an expr to put everything
;; around. Note that it does not check that the identifiers are unique.
(define (chain-lets [l : (listof (symbol * CExp))] [body : CExp]) : CExp
  (cond
    [(empty? l) body]
    [else (CLet (fst (first l)) (snd (first l)) (chain-lets (rest l) body))]))

(test (chain-lets (list (pair 'a (CNum 10))) (CNum 1))
  (CLet 'a (CNum 10) (CNum 1)))
(test (chain-lets (list (pair 'a (CNum 10)) (pair 'b (CNum 20))) (CNum 1))
  (CLet 'a (CNum 10) (CLet 'b (CNum 20) (CNum 1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Factored out Desugaring Branches ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; desugar-compare: desugars a comparison into sequenced primitive boolean operations.
;; overall idea is to compute the comparitors, store them in tmp positions, and then
;; do normal conjuctions with repetition of arguments.
;; PROBLEM? This does not work with short-circuiting.
(define (desugar-compare [ops : (listof symbol)]
                         [cmps : (listof PyExp)]
                         [lft : PyExp]) : CExp
  (local [(define ids (build-list (+ 1 (length cmps))
            (lambda (n) (string->symbol
                          (string-append "^id-" (to-string n))))))
          (define ids-exps (map (lambda (i) (CId i)) ids))]
    ;; the following is a little confusing. First we need to evaluate
    ;; everything, giving them all unique ids (^integer right now).
    ;; then we need to create all the binary comparisons, by shifting
    ;; and zipping, and finally we can turn that into a conjuction
    (chain-lets (zip pair ids (map desugar (cons lft cmps)))
      (foldl (lambda (a b) (CApp (CId '^and) (list a b))) (CBool true)
        (map (lambda (op-ids)
              (local [(define-values (op ids) op-ids)]
              (case op
                ;; Note: is using equals for Is valid?
                ['Is (CPrim 'equal (list (fst ids) (snd ids)))]
                ['Eq (CPrim 'equal (list (fst ids) (snd ids)))]
                ['NotEq (CPrim 'not (list (CPrim 'equal (list (fst ids) (snd ids)))))]
                ['Lt (CPrim '< (list (fst ids) (snd ids)))]
                ['LtE (CPrim '<= (list (fst ids) (snd ids)))]
                ['Gt (CPrim '> (list (fst ids) (snd ids)))]
                ['GtE (CPrim '>= (list (fst ids) (snd ids)))])))
        (zip pair ops (zip pair (init ids-exps) (rest ids-exps))))))))

;(test (desugar-compare (list 'Lt) (list (PyNum 1)) (PyNum 2))
;  (CLet '^id-0 (CNum 2)
;    (CLet '^id-1 (CNum 1)
;      (CApp (CId '^and) (list (CPrim '< (list (CId '^id-0) (CId '^id-1)))) (CBool true)))))
;
;(test (desugar-compare (list 'Lt 'Gt) (list (PyNum 1) (PyNum 3)) (PyNum 2))
;  (CLet '^id-0 (CNum 2)
;    (CLet '^id-1 (CNum 1)
;      (CLet '^id-2 (CNum 3)
;        (CApp (CId '^and)
;          (list
;            (CPrim '> (list (CId '^id-1) (CId '^id-2)))
;            (CPrim 'and
;              (list (CPrim '< (list (CId '^id-0) (CId '^id-1)))
;                    (CBool true)))))))))






