def divide(x, y):
    try:
        result = x / y
    except (ZeroDivisionError, UnboundLocalError):
        print("division by zero!")
    except Exception:
        print("exception")
    else:
        print("result is", result)
    finally:
        print("executing finally clause")

