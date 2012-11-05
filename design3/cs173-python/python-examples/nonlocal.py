def f():
    x=4
    def g():
        def h():
            nonlocal x
            x+=2
        h()
    g()
    print(x)
f()
