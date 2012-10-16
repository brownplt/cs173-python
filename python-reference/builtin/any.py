___assertEqual(any([None, None, None]), False)
___assertEqual(any([None, 4, None]), True)
___assertRaises(RuntimeError, any, [None, TestFailingBool(), 6])
___assertRaises(RuntimeError, all, TestFailingIter())
___assertRaises(TypeError, any, 10)               # Non-iterable
___assertRaises(TypeError, any)                   # No args
___assertRaises(TypeError, any, [2, 4, 6], [])    # Too many args
___assertEqual(any([]), False)                    # Empty iterator
S = [40, 60, 30]
___assertEqual(any(x > 42 for x in S), True)
S = [10, 20, 30]
___assertEqual(any(x > 42 for x in S), False)
