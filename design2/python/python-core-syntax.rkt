#lang plai-typed

(require (typed-in racket (string-join : ((listof string)
                                          string -> string))))

#|

This is the core language. In my core language, all "values" are objects. I have also defined a
variety of object singletons here (such as None, True, etc), and object makers (make-int, etc)
for convienience. Many of these functions really belong in the python-interp.rkt, but I was 
being lazy.

|#

; A location is a unique cell in memory
(define-type-alias Location number)

; NullLoc is a location that (new-loc) will never return. It is used as a
; placeholder by the desugarer so that it can create 'Var' objects (see below)
; without specifiying a Location. An alternative would be to have a different
; datatype that the desugarer uses that does not require it to specify a Location.
; I was being lazy here...
(define NullLoc -1)

; The store is a hash from Location to CObject (a value)
(define-type-alias Store (hashof Location CObject))

; A Var represents a single variable: it can be either a function
; parameter, a local, a non-local, or a global. The interpreter uses
; the type of the variable to determine how it should go about
; doing look-up and assignment operations.
(define-type Var
  ; a 'local' var that is assigned to locally and has a value. A value of
  ; 'NullLoc' for loc imples the variable is not currently assigned. In this case,
  ; the user will get an 'UnboundLocal' error.
  [Vlocal (loc : Location) (isparam : boolean)]

  ; a 'nonlocal' var points to a var at some other, non-local, scope
  [Vnonlocal (ptr : Scope)]

  ; a 'global' var points to a var at the global level
  [Vglobal (ptr : Scope)])

; VarType is used to pass information from the desugarer to the core about
; variables that appear within a function. The interpreter will convert these
; to type 'Var'.
(define-type VarType
  [VTlocal (isparam : boolean)]
  [VTnonlocal]
  [VTglobal])

(define-type-alias FuncVars (hashof symbol VarType))

; A hierarchy of Scope objects is the environment. There are two types of environments: a local
; enviroment, which has a pointer to a parent, non-local and possibily global, environment, and
; the global environment (which is the base case for this recursive data structure).
; Note that the 'db' hashes must be MUTABLE.
(define-type Scope
  [Slocal (db : (hashof symbol Var)) (parent : Scope)]
  [Sglobal (db : (hashof symbol Var))])

; expession syntax
(define-type CExp
  [CInt (n : number)]
  [CFloat (n : number)]
  [CComplex (n : number)]
  [CStr (s : string)]
  [CSeq (e1 : CExp) (e2 : CExp)]
  [CRaise (e1 : CExp)]
  [CIf (tst : CExp) (thn : CExp) (els : CExp)]
  [CId (x : symbol)]
  [CSetExp (x : symbol) (bind : CExp)]
  [CSetObj (x : symbol) (bind : CObject)]
  [CDel (x : symbol)]
  [CApp (fun : CExp) (args : (listof CExp))]
  [CFunc (args : (listof symbol))    ; function parameters, in order
         (vars : FuncVars)           ; all vars accessed by the function body
         (body : CExp)]              ; the function body
  [CReturn (value : CExp)]
  [CPrim1 (prim : symbol) (arg : CExp)]
;  [CPrim2 (prim : symbol) (left : CExp) (right : CExp)]
)

;; A answer from the interpreter. The store is threaded through each of
;; these answers
(define-type CAnswer
  ; AObject is the normal answer to an expression. It contains a value that
  ; the expression computed to.
  [AObject (value : CObject) (store : Store)]
  
  ; Interpreter threw an exception (that might be caught).
  [AException (exn : CObject) (store : Store)]

  ; TODO: Function Return, etc.
  [AReturn (value : CObject) (store : Store)]

  ; A Location answer is what can be returned by the lookup function.
  ; Warning: When I explained this to Joe, he did not like it. He thought
  ;          that since a location could only be returned by lookup, and not
  ;          the interpreter proper, lookup should have its own (define-type.
  [ALocation (loc : Location) (store : Store)]
)

;;
;; The Python Object Model
;;
;; Each primitive object type gets its own corresponding Racket type.
;; There are still plenty of details missing, here.
;;

(define-type CObject
  ; Represents an user-created instance of an object
  [OObject
   (class : CObject)  #| must be a OType object |#
   (dict : CObject)   #| every instance gets a dictionary |#
  ]
  
  ; Represents an instance of a type object (can be system or user created)
  [OType
   (class : CObject)  #| must be a OType object |#
   (dict : CObject)   #| must be a ODict |#
   (bases : CObject)  #| must be a OTuple of type objects |#
   (name : string)
  ]

  ; Represents an instance of a built-in immutable integer object
  [OInt
   (class : CObject)  #| must be a OType object |#
   (num : number)
  ]

  ; Represents an instance of a built-in immutable float object
  [OFloat
   (class : CObject)  #| must be a OType object |#
   (num : number)
  ]
  
  ; Represents an instance of a built-in immutable complex object
  [OComplex
   (class : CObject)  #| must be a OType object |#
   (num : number)
  ]
  
  ; Represents an instance of a built-in immutable string object
  [OStr
   (class : CObject)  #| must be a OType object |#
   (str : string)
  ]

  ; Represents an instance of a built-in dictionary object
  [ODict 
   (class : CObject)  #| must be a OType object |#
   (dict : (hashof symbol CObject))
  ]
  
  ; Represents an instance of a built-in tuple object
  [OTuple
   (class : CObject)  #| must be a OType object |#
   (tuple : (listof CObject))
  ]
  
  ; Represents a function value. 
  [OFunction
   (class : CObject)  #| must be a OType object |#
   (dict : CObject)   #| must be a ODict object |#
   (env : Scope)
   (args : (listof symbol))
   (vars : FuncVars)
   (body : CExp)
  ]
    
  ; Represents an exception
  [OException
   (class : CObject)  #| must be a OType object |#
   (dict : CObject)   #| must be a ODict object |#
   (args : (listof CObject))
  ]
   
  [OSelf] ; special marker to reference oneself. I did this to
          ; have a built-in search terminator to prevent infinite
          ; loops, but this may not be neccessary.
          ; TODO: revisit need
)

;;
;; is-subclass returns true iff:
;; 1.) A is the same object as B
;; 2.) B is in A.__bases__, or
;; 3.) is-subclass(Z,B) is true for any Z in A.__bases__.
;;
(define (is-subclass [a : CObject]
                     [b : CObject]) : boolean
  (let ([a-bases (OTuple-tuple (get-bases a))])
    (or (eq? a b)
        (or (member b a-bases)
            (is-any-subclass a-bases b)))))

;;
;; is-any-subclass returns true iff 
;; is-subclass(Z,b) is true for any Z in bases
;;
(define (is-any-subclass [bases : (listof CObject)]
                         [b : CObject])
  (cond
    [(empty? bases) false]
    [(is-subclass (first bases) b) true]
    [else (is-any-subclass (rest bases) b)]))
                          
        
;; is-instance returns true iff:
;; 1.) B is A.__class__, or
;; 2.) is-subclass(A.__class__,B) is true.
(define (is-instance [a : CObject]
                     [b : CObject]) : boolean
  (let ([a-class (get-class a)])
    (or (eq? b a-class)
        (is-subclass a-class b))))

;;
;; Gets __bases__ object of object o,
;; and guarantees that it is an oTuple object.
;;
(define (get-bases [o : CObject]) : CObject
  (let ([ob (type-case CObject o
              [OType (c d b n) b]
              [else (error 'get-bases "can only get bases of type object")])])
    (type-case CObject ob
      [OTuple (c t) ob]
      [else (error 'get-bases "bases is not a tuple?")])))
   
;;
;; Gets a class object of object o
;;
(define (get-class [o : CObject]) : CObject
  (let ([oc (type-case CObject o
              [OObject (c d) c]
              [OType (c d b n) c]
              [OInt (c n) c]
              [OComplex (c n) c]
              [OFloat (c n) c]
              [OStr (c s) c]
              [ODict (c d) c]
              [OTuple (c t) c]
              [OFunction (c d e a v b) c]
              [OException (c d a) c]
              [OSelf () (error 'get-class "got self")])])
    (type-case CObject oc
      [OSelf () o]
      [else oc])))

;;
;; Gets a dictionary object of object o
;;
(define (get-dict [o : CObject]) : (optionof CObject)
  (type-case CObject o
    [OObject (c d) (some d)]
    [OType (c d b n) (some d)]
    [OInt (c n) (none)]
    [OComplex (c n) (none)]
    [OFloat (c n) (none)]
    [OStr (c s) (none)]
    [ODict (c d) (none)] ; d here is not its "__dict__"
    [OTuple (c t) (none)]
    [OFunction (c d e a v b) (some d)]
    [OException (c d a) (some d)]
    [OSelf () (error 'get-class "got self")]))

;;
;; Get an attribute of an object
;;
#|(define (get-attr [o : CObject] [attr : string]) : CObject
  (type-case (optionof CObject) (get-dict o)
    [some (d) (ODict-]
    [none () (error 'get-attribute "object does not have a dictionary")]

      
    [OObject (c d) c]
    [OType (c d b n) c]
    [OInt (c n) (not (equal? n 0))]
    [OFloat (c n) (not (equal? n 0.0))]
    [OStr (c s) (not (equal? s ""))]
    [ODict (c d) (not (equal? (length (hash-keys d)) 0))]
    [OTuple (c t) (not (equal? (length (hash-keys d)) 0))]
    [OFunction (c d e a v b) c]
    [OSelf () (error 'get-class "got self")]))
|#

;;
;; Truth-value
;;
(define (truth-value [o : CObject]) : boolean
  (type-case CObject o
    [OObject (c d) true]
    [OType (c d b n) true]
    [OInt (c n) (not (equal? n 0))]
    [OFloat (c n) (not (equal? n 0))]
    [OComplex (c n) (not (equal? n 0))]
    [OStr (c s) (not (equal? s ""))]
    [ODict (c d) (not (equal? (length (hash-keys d)) 0))]
    [OTuple (c t) (not (equal? (length t) 0))]
    [OFunction (c d e a v b) true]
    [OException (c d a) true]
    [OSelf () (error 'get-class "got self")]))
 
;;
;; Cyclic type primitives
;;

(define py-tuple
  'dummy)

(define py-dict
  'dummy)

(define py-type
  'dummy)

(define py-object
  'dummy)
  
;;
;; Cyclic helpers to make objects which are instances of the
;; built-in primitive types
;;

(define (make-dict [h : (hashof symbol CObject)])
  'dummy) ; this is used in bootstrapping, so we must set! in (shared

(define (make-tuple [i : (listof CObject)])
  'dummy) ; this is used in bootstrapping, so we must set! in (shared

;;
;; Ok, let's build this beast inside
;; a (shared...) clause.
;;

(shared ([vpy-type
          (OType (OSelf)
                 (vmake-dict (make-hash (list)))
                 (vmake-tuple (list vpy-object))
                 "type")]

         [vpy-object
          (OType vpy-type
                 (vmake-dict (make-hash (list)))
                 (vmake-tuple (list))
                 "object")]
                  
         [vpy-tuple
          (OType vpy-type 
                 (vmake-dict (make-hash (list)))
                 (vmake-tuple (list vpy-object))
                 "tuple")]
         
         [vpy-dict
          (OType vpy-type 
                 (vmake-dict (make-hash (list))) 
                 (vmake-tuple (list vpy-object))
                 "dict")]

         [vmake-dict
          (lambda (h)
            (ODict vpy-dict h))]
         
         [vmake-tuple
          (lambda (i)
            (OTuple vpy-tuple i))]
         
         ) #| end shared |#
  (begin
    (set! py-type vpy-type)
    (set! py-object vpy-object)
    (set! py-tuple vpy-tuple)
    (set! py-dict vpy-dict)
    (set! make-dict vmake-dict)
    (set! make-tuple vmake-tuple)
    ))

;;
;; Instance Types
;;

(define py-NoneType
  (OType py-type
         (make-dict (make-hash (list)))
         (make-tuple (list py-object))
         "NoneType"))

(define py-NotImplementedType
  (OType py-type
         (make-dict (make-hash (list)))
         (make-tuple (list py-object))
         "NotImplementedType"))

(define py-str
  (OType py-type
         (make-dict (make-hash (list)))
         (make-tuple (list py-object))
         "string"))

(define py-float
  (OType py-type
         (make-dict (make-hash (list)))
         (make-tuple (list py-object))
         "float"))

(define py-int
  (OType py-type
         (make-dict (make-hash (list)))
         (make-tuple (list py-object))
         "int"))

(define py-complex
  (OType py-type
         (make-dict (make-hash (list)))
         (make-tuple (list py-object))
         "complex"))

(define py-bool
  (OType py-type
         (make-dict (make-hash (list)))
         (make-tuple (list py-int))
         "bool"))

(define py-function
  (OType py-type
         (make-dict (make-hash (list)))
         (make-tuple (list py-object))
         "function"))

(define py-BaseException
  (OType py-type
         (make-dict (make-hash (list)))
         (make-tuple (list py-object))
         "BaseException"))

(define py-Exception
  (OType py-type
         (make-dict (make-hash (list)))
         (make-tuple (list py-BaseException))
         "Exception"))

(define py-NameError
  (OType py-type
         (make-dict (make-hash (list)))
         (make-tuple (list py-Exception))
         "NameError"))

(define py-TypeError
  (OType py-type
         (make-dict (make-hash (list)))
         (make-tuple (list py-Exception))
         "TypeError"))

(define py-UnboundLocalError
  (OType py-type
         (make-dict (make-hash (list)))
         (make-tuple (list py-Exception))
         "UnboundLocalError"))

; UndefinedType is a (currently unused) idea. Ignore me. 
;(define py-UndefinedType
;  (OType py-type
;         (make-dict (make-hash (list)))
;         (make-tuple (list py-Exception))
;         "UndefinedType"))

;;
;; Instance Objects
;;

(define py-True
  (OInt py-bool 1))

(define py-False
  (OInt py-bool 0))

(define py-None
  (OObject py-NoneType (make-dict (make-hash (list)))))

(define py-NotImplemented
  (OObject py-NotImplementedType (make-dict (make-hash (list)))))

; Undefined is (an unused reference) for internal use only
;(define py-Undefined
;  (OObject py-UndefinedType (make-dict (make-hash (list)))))

;;
;; Helpers to make objects of various types
;; Some of these are factories which cache values
;;

(define make-int
  (let ([all-ints (make-hash (list))])
    (lambda ([num : number]) : CObject
      (type-case (optionof CObject) (hash-ref all-ints num)
        [some (v) v]
        [else (let ([v (OInt py-int num)])
                (begin
                  (hash-set! all-ints num v)
                  v))]))))
  
(define make-float
  (let ([all-floats (make-hash (list))])
    (lambda ([num : number]) : CObject
      (type-case (optionof CObject) (hash-ref all-floats num)
        [some (v) v]
        [else (let ([v (OFloat py-float num)])
                (begin
                  (hash-set! all-floats num v)
                  v))]))))

(define make-complex
  (let ([all-complexes (make-hash (list))])
    (lambda ([num : number]) : CObject
      (type-case (optionof CObject) (hash-ref all-complexes num)
        [some (v) v]
        [else (let ([v (OComplex py-complex num)])
                (begin
                  (hash-set! all-complexes num v)
                  v))]))))

(define (make-bool [o : CObject]) : CObject
  (if (truth-value o)
      py-True
      py-False))

(define (make-str [str : string]) : CObject
  (OStr py-str str))

(define (make-function [env : Scope]
                       [args : (listof symbol)]
                       [vars : FuncVars]
                       [body : CExp]) : CObject
  (OFunction py-function 
             (make-dict (make-hash (list)))
             env
             args
             vars
             body))

(define (make-BaseException [args : (listof CObject)])
  (OException py-BaseException (make-dict (make-hash (list))) args))

(define (make-Exception [args : (listof CObject)])
  (OException py-Exception (make-dict (make-hash (list))) args))

(define (make-NameError [args : (listof CObject)])
  (OException py-NameError (make-dict (make-hash (list))) args))

(define (make-TypeError [args : (listof CObject)])
  (OException py-TypeError (make-dict (make-hash (list))) args))

(define (make-UnboundLocalError [args : (listof CObject)])
  (OException py-UnboundLocalError (make-dict (make-hash (list))) args))

;;
;; Returns a pretty string for an object
;;
;; TODO: In a proper world, this function should call __str__ on 
;; the object, instead of generating the string itself.
;;
(define (object-str [o : CObject]) : string
  (type-case CObject o
    [OObject (c d)
             (cond
               [(eq? o py-None) "None"]
               [(eq? o py-NotImplemented) "NotImplemented"]
               [else "<instance object>"])]
    
    [OType (c d b n) 
           (string-append "<class '"
                          (string-append n "'>"))]
    
    [OInt (c n)
          (cond
            [(eq? o py-True) "True"]
            [(eq? o py-False) "False"]
            [else (to-string n)])]
    
    [OFloat (c n) 
            (to-string n)]
    
    [OComplex (c n)
              (to-string n)]
    
    [OStr (c s)
          s]
    
    [ODict (c d)
           (string-append
            "{"
            (string-append
             (string-join (map (lambda (k) : string
                                 (string-append
                                  (symbol->string k)
                                  (string-append 
                                   ": "
                                   (object-str (some-v (hash-ref d k))))))
                               (hash-keys d))
                          ", ")
             "}"))]
    
    [OTuple (c t) (string-append "(" (string-append (string-join (map object-str t) ", ") ")"))]
    
    [OFunction (c d e a v b) "<function: TBD>"]
    
    [OException (c d a) (string-append
                         (OType-name c)
                         (string-append
                          "("
                          (string-append
                           (string-join
                            (map object-str a) ", ") 
                           ")")))]
    
    [OSelf () "<self-pointer>"]))

;;
;; Returns a representation string for an object
;;
;; TODO: In a proper world, this function should call __repr__ on 
;; the object, instead of generating the string itself.
;;
(define (object-repr [o : CObject]) : string
  (type-case CObject o
    [OStr (c s)
          (string-append "'" (string-append s "'"))]
    [else (object-str o)]))
