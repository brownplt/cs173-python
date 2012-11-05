#Should produce an UnboundLocal assertion
#TODO: catch this exception

def foo():
    print(x)
    x = 3

foo()

