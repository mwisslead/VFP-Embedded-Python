SET PROCEDURE TO python

DO start_python IN python

dict = CREATEOBJECT('PythonDictionary')

dict.SetItem('num', 15.3)
dict.SetItem('bool', .T.)
dict.SetItem('None', .NULL.)
dict.SetItem('string', 'Words and stuff')
dict.SetItem('date', DATE())
dict.SetItem('datetime', DATETIME())
?dict.repr()
dict.DelItem('None')
?dict.repr()

FOR EACH dict_key IN dict.iter
   ?dict.GetItem(dict_key)
ENDFOR

DIMENSION testarray(3)
testarray(1) = 15.3
testarray(2) = .T.
testarray(3) = 'Words and stuff in a list'

listofstuff = CREATEOBJECT('PythonList', @testarray)
RELEASE testarray

listofstuff.callmethod('append', CREATEOBJECT('PythonTuple', .NULL.))

FOR EACH listofstuff_item IN listofstuff.iter
   ? 'array item', listofstuff_item
ENDFOR

DIMENSION testarray(5)
listofstuff = CREATEOBJECT('PythonList', @testarray)
RELEASE testarray

?listofstuff.repr()
listofstuff.SetItem(1, 15.3)
listofstuff.SetItem(2, .T.)
listofstuff.SetItem(3, .NULL.)
listofstuff.SetItem(4, 'Words and stuff')
listofstuff.SetItem(5, DATE())
listofstuff.SetItem(6, DATETIME())
?listofstuff.repr()
listofstuff.DelItem(3)
?listofstuff.repr()

FOR EACH listofstuff_item IN listofstuff.iter
   ? 'array item', listofstuff_item
ENDFOR

?PyNone.Repr()
?PyBuiltins.Repr()

DO stop_python IN python2
