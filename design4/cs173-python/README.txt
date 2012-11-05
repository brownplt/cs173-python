Overall Design
--------------

Think, desugaring set to MAX. CPS is used heavily. The language we
interpret has only primitive types, primitive ops, if, let, set, seq, lambdas,
and function application.

See LANGUAGES.txt for a description of the languages (we have three - a
surface, a core, and a micro-core).

Current Status
------------------

Right now this is a skeleton. I ripped out and rewrote pretty much everything
after the first checkpoint, and so it passes NO tests currently (that I'm
aware of). The reason for this is that it was a pretty massive overhaul to get
let/cc in, and I haven't gotten around to fixing the trivial examples. It
shouldn't actually be that much work to get lots of tests working - while loops
with break/continue are working, functions / early return are almost working,
exceptions are implemented but untested. There is a file python-tests.rkt that
shows a few high-level examples, and there are various tests in code.

Bugs/Limitations
----------------

There is a bug that prevents how USet (mutation) works, so that the mutations
are trapped inside the closure environment, and since the simplistic way I'd
implemented python FunctionDefs uses mutation, they won't work (and of course,
mutation won't work). This also doesn't currently handle any python scoping
rules, though that should be a pretty easy desugar (hoist into lets with an
Unbound value, through an error if it hasn't been modified).


Implementation Notes
--------------------

1. For identifiers that should are created with desugaring, I adopt the
convention of preceeding them with a carat (^). This is a legal character
for identifiers in racket but not in python, so it will make all identifiers
not collide.


