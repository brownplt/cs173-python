def f(): pass
class C: pass
x = C()
if not f: raise ('f is false instead of true')
if not C: raise ('C is false instead of true')
if not x: raise ('x is false instead of true')
