#lang plai-typed

(define-type PyExp
  [PyNum (n : number)]
  [PyTuple (l : (listof PyExp))]
  [PySeq (es : (listof PyExp))]
  [PyId (x : symbol)]
  [PySet! (id : symbol) (value : PyExp)]
  [PyApp (fun : PyExp) (args : (listof PyExp)) (starargs : PyExp)]
  [PyFunc (args : (listof symbol)) (vararg : (optionof symbol)) (body : PyExp)]
  [PyReturn (value : PyExp)]
  [PyIf (test : PyExp) (then : PyExp) (else : PyExp)]
  [PyOp (id : symbol) (args : (listof PyExp))]
  [PyPass])

(define someF some)
(define noneF none)

