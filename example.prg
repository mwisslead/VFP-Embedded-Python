SET PROCEDURE TO python ADDITIVE

start_python()

args = CREATEOBJECT('PythonTuple', 'arg1', 2)
?args.repr()
retval = PythonFunctionCall('example_module', 'swap_args', args)
?retval.repr()
r1 = retval.getitem(0)
r2 = retval.getitem(1)
?r1, r2
RELEASE r1, r2, retval

args = CREATEOBJECT('PythonTuple', 'arg1')
kwargs = CREATEOBJECT('PythonDictionary')
kwargs.setItem('arg2', 2)
?args.repr()
?kwargs.repr()
retval = PythonFunctionCall('example_module', 'swap_args', args, kwargs)
?retval.repr()
r1 = retval.getitem(0)
r2 = retval.getitem(1)
?r1, r2
RELEASE kwargs, r1, r2, retval

example_dict = CREATEOBJECT('PythonDictionary')
example_dict.setItem('key1', .NULL.)
example_dict.setItem('key2', .T.)
example_dict.setItem('key3', 3.5)
example_dict.setItem('key4', DATE())
?example_dict.repr()
?example_dict.getitem('key4')
RELEASE example_dict

example_list = CREATEOBJECT('PythonList')
example_list.callMethod('append', CREATEOBJECT('PythonTuple', .NULL.))
example_list.callMethod('append', CREATEOBJECT('PythonTuple', .T.))
example_list.callMethod('append', CREATEOBJECT('PythonTuple', 3.5))
example_list.callMethod('append', CREATEOBJECT('PythonTuple', DATETIME()))
?example_list.repr()
?example_list.getitem(3)
example_list.setitem(2, 3.6)
?example_list.repr()
?example_list.getitem(2)
RELEASE example_list

&&generate an error
example_list = CREATEOBJECT('PythonList')
?example_list.getitem(0)
?example_list.setitem(0, 5)
RELEASE example_list

stop_python()

RELEASE PROCEDURE python

RETURN