#class MyClass:
#    a = "hello"
#    def f(self):
#        print("self.a: ")
#        print(self.a);
#        print(type(self))
#
#    class MySubClass:
#        nonlocal a
#        b = "world"
#        def f(self):
#            print("self.a: ")
#            print(self.a);
#            print("self.b: ")
#            print(self.b);
#            print(type(self))
#
#c = MyClass.MySubClass();
#c.f();

class TestClass:
    a = "hello"

print(type(TestClass))

def my():
    def foo():
        def bar():
            global x
            print(x)

my()

