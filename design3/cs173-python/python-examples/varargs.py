x = (1,2,3)

def f(x,y,*z):
    print(x+y)
    print(z)

f(1,*x)
