# should produce: SyntaxError: no binding for nonlocal 'a' found
# at time of function definition

def foo(foo_arg):
    def bar(bar_arg):
        nonlocal a

#foo(0);

print("should not get here")
