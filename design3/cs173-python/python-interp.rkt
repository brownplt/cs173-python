#lang plai-typed

(require "python-core-syntax.rkt" 
         "python-monad.rkt"
         "python-lib.rkt"
         (typed-in racket/base
                   [display : (string -> void)]
                   [andmap : (('a -> boolean) (listof 'a) -> boolean)]))

;;note: interp and primitives need to be mutually recursive -- primops
;;need to lookup and apply underscore members (ie + calls __plus__),
;;and applying a function requires m-interp.  Since racket doesn't
;;allow mutually recursive modules, I had to move python-primitives
;;into python-interp.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             Primitives          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;note: primitive functions have type (listof CVal) -> (PM CVal)
;;they take in a list of vals because they may take in an arbitrary
;;number of values (think print).  At some point, I will add in better
;;argument checking to prim functions, but at the moment, I just
;;assume they are correct

;;helper macro for (define-primf)
(define-syntax prim-bind
  (syntax-rules (&)
    [(prim-bind val (& (bind pred)) body)
     (let ([bind val])
       (if (pred bind)
           body
           (interp-error "primf given wrong argument type")))]
    [(prim-bind val (& bind) body)
     (let ([bind val])
       body)]
    [(prim-bind val ((bind pred) binds ...) body)
     (let ([val-val val])
       (if (empty? val-val)
           (interp-error "primf not given enough arguments")
           (let ([bind (first val-val)])
             (if (pred bind)
                 (prim-bind (rest val-val) (binds ...) body)
                 (interp-error "primf given wrong argument type")))))]
    [(prim-bind val (bind binds ...) body)
     (let ([val-val val])
       (if (empty? val-val)
           (interp-error "primf not given enough arguments")
           (let ([bind (first val-val)])
             (prim-bind (rest val-val) (binds ...) body))))]
    [(prim-bind val () body)
     (if (empty? val)
         body
         (interp-error "primf given too many arguments"))]))

;;defines the racket version of a primf.
;;syntax: (define-primf (name . args) body)
;;args: (arg ... & rest)
;;arg (including rest): id or (id predicate)
;;notes: & rest is optional, bound to remaining arguments
;;actual racket function's type is ((listof CVal) -> (PM CVal))
;;if predicate is present, define-primf throws an error unless the
;;predicate returns true when applied to its associated arg
(define-syntax define-primf
  (syntax-rules ()
    [(define-primf (name . args) body)
     (define (name (arg : (listof CVal))) : (PM CVal)
       (prim-bind arg args body))]))

;;get the class of obj
(define-primf (class obj)
  (type-case CVal obj
    [VUndefined () (interp-error "local used before being set")]
    [VNone () (get-global "none-type")]
    [VTrue () (get-global "bool-type")]
    [VFalse () (get-global "bool-type")]
    [VNum (n) (get-global "num-type")]
    [VStr (s) (get-global "str-type")]
    [VBox (v) (interp-error "Boxes don't have a class")]
    [VObj (dict class) (get-box (list class))]
    [VPrimF (id) (get-global "func-type")]
    [VPrimMap (m) (interp-error "prim maps don't have a class")]
    [VClosure (e a v b) (get-global "func-type")]
    [VTuple (l) (get-global "tuple-type")]))

;;get the dict out of obj
(define (dict (obj : CVal)) : (PM CVal)
  (type-case CVal obj
    [VObj (dict class) (get-box (list dict))]
    [else (m-return (VNone))]))

;;get the super class of the class c (c's class must be type)
(define (super (c : CVal)) : (PM CVal)
  (m-do ([c-c (class (list c))]
         [class-type (get-global "class-type")]
         [(if (eq? c-c class-type)
              (pm-catch-error
               (local-lookup c (VStr "__super__"))
               (lambda (error)
                 (get-global "obj-type")))
              (m-do ([c-s (pretty c)]
                     [(interp-error (string-append "isn't a class:"
                                                   c-s))])))])))

;;add a value to a prim dict (return the new dict)
(define (prim-dict-add (c-dict : CVal) (key : CVal) (val : CVal)) : (PM CVal)
  (type-case CVal c-dict
    [VPrimMap (m)
              (m-return (VPrimMap (hash-set m key val)))]
    [else (interp-error "prim-dict-add expects a prim-dict")]))

;;lookup a value in a prim dict
(define (prim-dict-lookup (c-dict : CVal) (key : CVal)) : (PM CVal)
  (type-case CVal c-dict
    [VPrimMap (m)
              (type-case (optionof CVal) (hash-ref m key)
                [some (v) (m-return v)]
                [none () (interp-error
                          (string-append "key not found: "
                                         (to-string key)))])]
    [else (interp-error
           (string-append "prim-dict-lookup wants a dict: "
                          (to-string key)))]))

;;lookup a value in an object (fails if there is no dict)
(define (local-lookup (obj : CVal) (key : CVal)) : (PM CVal)
  (m-do ([c-dict (dict obj)]
         [(prim-dict-lookup c-dict key)])))

;;takes an object in the first position and a key in the second
;;position, and returns true iff the object's class (or its
;;superclasses) contains key
(define-primf (class-has-member? & args)
  (pm-catch-error (m-do ([(class-lookup args)])
                        (VTrue))
                  (lambda (x)
                    (m-return (VFalse)))))

;;takes an object in the first position and a key in the second
;;position, returns the value that the class has for key. Errors if
;;the key is not present in the class or its superclasses.  Function
;;return values are curried with the object (so if the class contains
;;(lambda (this) this), the returned function is equivalent to (lamba ()
;;this), where this refers to the object.  This sets up obj.method()
;;semantics properly
(define-primf (class-lookup object name)
  (m-do
   ([obj-type (get-global "obj-type")]
    [partial (get-global "partial-apply")]
    [(local [(define (iter c)
               (m-do ([c-dict (dict c)]
                      [(pm-catch-error
                        (local-lookup c name)
                        (lambda (error)
                          (if (eq? c obj-type)
                              (pm-error error)
                              (m-bind (super c)
                                      iter))))])))]
            (m-do ([c (class (list object))]
                   [res (iter c)]
                   [(if (or (VClosure? res)
                            (VPrimF? res))
                        (apply-func partial
                                    (list res object)
                                    (VTuple empty))
                        (m-return res))])))])))

;;take an object in the first position and a key in the second
;;position.  looks the key up in the objects dict first, and in the
;;objects class second (if the key isn't in the dict)
(define-primf (obj-lookup obj name)
  (cond
     [(equal? name (VStr "__dict__")) (dict obj)]
     [(equal? name (VStr "__class__")) (class (list obj))]
     [else (pm-catch-error
            (local-lookup obj name)
            (lambda (error)
              (class-lookup (list obj name))))]))

;;will be replaced by a to-string method in classes
(define (pretty arg)
  (type-case CVal arg
    [VNum (n) (m-return (to-string n))]
    [VNone () (m-return "None")]
    [VStr (s) (m-return s)]
    [VTuple (l) (m-do ([vals (m-map pretty l)]
                       [rvals (m-return (reverse vals))])
                      (cond
                       [(empty? rvals) "()"]
                       [(empty? (rest rvals))
                        (string-append "("
                                       (string-append (first rvals)
                                                      ",)"))]
                       [else (string-append
                              "("
                              (string-append
                               (foldl (lambda (c t)
                                        (string-append c
                                                       (string-append ", "
                                                                      t)))
                                      (first rvals)
                                      (rest rvals))
                               ")"))]))]
    [VObj (c dict)
          (m-do ([c (get-box (list c))]
                 [c-s (pretty c)]
                 [dict (get-box (list dict))]
                 [dict-s (pretty dict)])
                (string-append "(obj "
                               (string-append c-s
                                              (string-append " "
                                                             (string-append dict-s
                                                                            ")")))))]
    [else (m-return (to-string arg))]))

;;gets the global variable dict
(define get-globals
  (pm-lookup-store -2))

;;sets the global variable dict
(define (set-globals (v : CVal))
  (type-case CVal v
    [VPrimMap (m) (pm-add-store -2 v)]
    [else (interp-error "globals must be a prim dict")]))

;;gets a particular global from the global varaible dict
(define (get-global (arg : string))
  (m-do ([d get-globals]
         [(prim-dict-lookup d (VStr arg))])))

;;adds a global variable and its value to the global variable dict
(define (add-global (name : string) (val : CVal))
  (m-do ([d get-globals]
         [new-d (prim-dict-add d (VStr name) val)]
         [(set-globals new-d)])))

;;prints the args, separated by spaces, followed by a newline
(define-primf (print val & rest)
  (m-do ([prettied (pretty val)]
         [(m-return (display prettied))]
         [(if (empty? rest)
              (begin (display "\n")
                     (m-return (VNone)))
              (begin (display " ")
                     (print rest)))])))

;;checks whether the 2 arguments are equal
(define-primf (equal left right)
  (if (equal? left
              right)
      (m-return (VTrue))
      (m-return (VFalse))))

;;numeric addition
(define-primf (add left right)
  (m-return (VNum
             (+ (VNum-n left)
                (VNum-n right)))))

;;numeric subtraction
(define-primf (sub left right)
  (m-return (VNum
             (- (VNum-n left)
                (VNum-n right)))))

;;numeric negation
(define-primf (neg arg)
  (m-return (VNum (- 0 (VNum-n arg)))))

;;gets a value from a box
(define-primf (get-box (box VBox?))
  (pm-lookup-store (VBox-v box)))

;;sets the value inside a box
(define-primf (set-box (box VBox?) val)
  (m-do ([(pm-add-store (VBox-v box) val)])
        val))

;;appends n tuples
(define-primf (tuple-append & (args (lambda (args) (andmap VTuple? args))))
  (m-return
   (VTuple
    (foldr (lambda (t l)
             (append (VTuple-l t)
                     l))
           empty
           args))))

;;finds the length of a tuple
(define-primf (tuple-length (t VTuple?))
  (m-return (VNum (length (VTuple-l t)))))

;;finds the appropriate racket function for a given VPrimF symbol
(define (python-prim op) : ((listof CVal) -> (PM CVal))
  (case op
    [(print) print]
    [(equal) equal]
    [(int-add) add]
    [(int-sub) sub]
    [(int-neg) neg]
    [(get-box) get-box]
    [(set-box) set-box]
    [(class-has-member?) class-has-member?]
    [(class-lookup) class-lookup]
    [(tuple-append) tuple-append]
    [(tuple-length) tuple-length]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            interp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;returns the first n elements in lst
(define (take n lst)
  (cond
   [(or (empty? lst)
        (= n 0)) empty]
   [else (cons (first lst)
               (take (- n 1)
                     (rest lst)))]))

;;returns lst without the first n elements
(define (drop n lst)
  (cond [(empty? lst) empty]
        [(= n 0) lst]
        [else (drop (- n 1) (rest lst))]))

;;applies funct to args and varargs
(define (apply-func (func : CVal) (args : (listof CVal)) (varargs : CVal)) : (PM CVal)
  (let ([args (append args
                      (VTuple-l varargs))])
    (type-case CVal func
      [VClosure
       (c-env off-args off-vararg body)
       (let ([named-args (take (length off-args) args)]
             [varargs (drop (length off-args) args)])
         (if (or (< (length args)
                    (length off-args))
                 (and (not (empty? varargs))
                      (none? off-vararg)))
             (interp-error
              (string-append "Application failed with arity mismatch\nfunction: "
                             (string-append (to-string func)
                                            (string-append "\nargs: "
                                                           (to-string args)))))
             (m-do ([new-env
                     (m-foldl
                      (lambda (pair env)
                        (local [(define-values (name val) pair)]
                               (m-do ([loc (add-new-loc val)])
                                     (hash-set env name loc))))
                      (type-case (optionof symbol) off-vararg
                        [none () (m-return c-env)]
                        [some (name)
                              (m-do ([loc (add-new-loc (VTuple varargs))])
                                    (hash-set c-env name loc))])
                      (map2 (lambda (x y)
                              (values x y))
                            off-args named-args))]
                    [(pm-catch-return (m-do ([(m-interp body
                                                        new-env)])
                                            (VNone))
                                      m-return)]))))]
      [VPrimF (id) ((python-prim id) args)]
      [else (interp-error (string-append "Applied a non-function: "
                                         (to-string func)))])))

(define (m-interp expr env) : (PM CVal)
  (type-case CExp expr
    [CUndefined () (m-return (VUndefined))]
    [CNone () (m-return (VNone))]
    [CTrue () (m-return (VTrue))]
    [CFalse () (m-return (VFalse))]
    [CNum (n) (m-return (VNum n))]
    [CStr (s) (m-return (VStr s))]
    [CBox (v) (m-do ([val (m-interp v env)]
                     [loc (add-new-loc val)])
                    (VBox loc))]
    [CObj (d c)
          (m-do ([dict (m-interp d env)]
                 [class (m-interp c env)])
                (VObj dict class))]
    [CPrimMap (vals)
              (m-do ([contents
                      (m-map (lambda (pair)
                               (local [(define-values (key val) pair)]
                                      (m-do ([key (m-interp key env)]
                                             [val (m-interp val env)])
                                            (values key val))))
                             vals)])
                    (VPrimMap
                     (hash contents)))]
    [CTuple (l)
            (m-do ([contents (m-map (lambda (v)
                                      (m-interp v env))
                                    l)])
                  (VTuple contents))]

    [CId (x)
         (type-case (optionof Location) (hash-ref env x)
           [some (l)
                 (m-do ([store pm-get-store]
                        [(let ([v (type-case (optionof CVal) (hash-ref store l)
                                    [some (v) v]
                                    [none () (error 'interp
                                                    (string-append
                                                     "can't find loc for var: "
                                                     (to-string x)))])])
                           (if (VUndefined? v)
                               (interp-error (string-append "local used before it was defined: "
                                                            (to-string x)))
                               (m-return v)))]))]
           [none () (interp-error (string-append "Unbound identifier: "
                                                 (to-string x)))])]
    [CSet! (id v)
           (m-do ([v (m-interp v env)]
                  [(type-case (optionof Location) (hash-ref env id)
                     [some (l) (pm-add-store l v)]
                     [none () (error 'interp (string-append "variable never bound:"
                                                            (to-string id)))])]))]
    [CLet (x bind body)
          (m-do ([val (m-interp bind env)]
                 [loc (add-new-loc val)]
                 [(m-interp body (hash-set env x loc))]))]
    [CAddGlobal (id bind)
                (m-do ([bind (m-interp bind env)]
                       [(add-global (symbol->string id) bind)])
                      bind)]
    [CSeq (e1 e2)
          (m-do ([(m-interp e1 env)]
                 [(m-interp e2 env)]))]

    [CFunc (args vararg body)
           (m-return (VClosure env args vararg body))]
    [CApp (func args varargs)
          (m-do ([func (m-interp func env)]
                 [args (m-map (lambda (arg) (m-interp arg env)) args)]
                 [varargs (m-interp varargs env)]
                 [(apply-func func args varargs)]))]
    [CReturn (v)
             (m-do ([v (m-interp v env)]
                    [(pm-return v)]))]
    [CPrimF (id) (m-return (VPrimF id))]
    [CIf (test t e)
         (m-do ([test-v (m-interp test env)]
                [(if (equal? test-v (VFalse))
                     (m-interp e env)
                     (m-interp t env))]))]
    [CError (val)
            (m-bind (m-interp val env) pm-error)]
    ;;[else (error 'm-interp (string-append "not implemented: "
    ;;                                      (to-string expr)))]
    ))

(define (interp expr)
  (local [(define-values (store res)
            ((m-interp expr (hash (list)))
             empty-store))]
         (type-case (ROption CVal) res
           [RValue (v) v]
           [RReturn (v) (error 'interp "returned when not in function context")]
           [RError (v) (error 'interp (to-string v))])))

