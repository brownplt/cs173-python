if 'xyz' + 'abcde' != 'xyzabcde': raise ('string concatenation')
if 'xyz'*3 != 'xyzxyzxyz': raise ('string repetition *3')
if 0*'abcde' != '': raise ('string repetition 0*')
if min('abc') != 'a' or max('abc') != 'c': raise ('min/max string')
if 'a' in 'abc' and 'b' in 'abc' and 'c' in 'abc' and 'd' not in 'abc': pass
else: raise ('in/not in string')
