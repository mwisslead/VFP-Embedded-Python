LPARAMETERS PythonDirectory

LOCAL PathToSet
PathToSet = ADDBS(JUSTPATH(SYS(16)))

SET DEFAULT TO (PathToSet)
SET PROCEDURE TO (PathToSet + 'python.prg')
start_python(PythonDirectory)

SET ASSERT ON

PythonFunctionCall('logging', 'basicConfig', PyEmptyTuple)
Assert PyBuiltins.callmethod('str', CREATEOBJECT('pythontuple', 3)) == '3'
IF PyMajorVersion == 2
   Assert PyBuiltins.callmethod('unicode', CREATEOBJECT('pythontuple', 3)) == '3'
ENDIF

Assert PyBuiltins.callmethod('round', CreateObject('pythontuple', 3.5)) == 4
Assert PyBuiltins.callmethod('max', CreateObject('pythontuple', 3.5, 3)) == 3.5

Local Now, NowTuple
Now = DateTime()
NowTuple = CreateObject('PythonTuple', Now, TTOD(Now))

ASSERT NowTuple.getitem(0) == Now
ASSERT NowTuple.getitem(1) == TTOD(Now)

?NowTuple.repr()

try
   PythonFunctionCall('example_module', 'error', CreateObject('PythonTuple', .NULL.))
   assert .f.
catch to oerr
   ?oerr.message
endtry

?RTRIM(pystderr.read(), 0, CHR(10), CHR(13))

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

try
   ?example_list.getitem(0)
catch to oerr
   ?oerr.message
   ?RTRIM(pystdout.read(), 0, CHR(10), CHR(13))
   ?RTRIM(pystderr.read(), 0, CHR(10), CHR(13))
ENDTRY

try
   ?example_list.setitem(0, 5)
catch to oerr
   ?oerr.message
   ?RTRIM(pystdout.read(), 0, CHR(10), CHR(13))
   ?RTRIM(pystderr.read(), 0, CHR(10), CHR(13))
ENDTRY

RELEASE example_list

PyBuiltins.callmethod('print', CreateObject('pythontuple', PI()))

?RTRIM(pystdout.read(), 0, CHR(10), CHR(13))

assert PythonFunctionCall('decimal', 'Decimal', CreateObject('pythontuple', '1.57')) == 1.57
