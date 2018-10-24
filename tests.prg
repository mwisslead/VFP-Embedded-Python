LOCAL PathToSet
PathToSet = JUSTPATH(SYS(16))

SET DEFAULT TO (PathToSet)
SET PATH TO (PathToSet)

SET PROCEDURE TO python.prg
start_python()

SET ASSERT ON

Assert PyBuiltins.callmethod('str', CREATEOBJECT('pythontuple', 3)) == '3'
Assert PyBuiltins.callmethod('unicode', CREATEOBJECT('pythontuple', 3)) == '3'

Assert PyBuiltins.callmethod('round', CreateObject('pythontuple', 3.5)) == 4
Assert PyBuiltins.callmethod('max', CreateObject('pythontuple', 3.5, 3)) == 3.5

Local Now, NowTuple
Now = DateTime()
NowTuple = CreateObject('PythonTuple', Now, TTOD(Now))

ASSERT NowTuple.getitem(0) == Now
ASSERT NowTuple.getitem(1) == TTOD(Now)

?NowTuple.repr()
