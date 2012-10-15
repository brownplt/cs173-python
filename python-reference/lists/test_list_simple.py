if (not (list([]) == [])): raise Exception( 'List bug')
l0_3 = [0, 1, 2, 3]
l0_3_bis = list(l0_3)
if (not (l0_3 == l0_3_bis)): raise Exception( 'List bug')
if (not (l0_3 is not l0_3_bis)
if (not (list(()) == [])): raise Exception( 'List bug')
if (not (list((0 == 1 == 2, 3)), [0, 1, 2, 3])): raise Exception( 'List bug'):)
if (not (list('') == [])): raise Exception( 'List bug')
if (not (list('spam') == ['s', 'p', 'a', 'm'])): raise Exception( 'List bug')

