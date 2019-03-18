DEFINE CLASS PythonObjectImpl AS Custom
   PROTECTED pyobject
   iter = .NULL.

   PROCEDURE INIT(pyobj)
      IF pyobj == 0
         ERROR 'invalid input value'
         RETURN .NULL.
      ENDIF
      this.pyobject = pyobj
   ENDPROC

   PROCEDURE obj
      RETURN this.pyobject
   ENDPROC

   PROCEDURE GETVAL
      LOCAL retval, typeobj
      IF this.pyobject > 0
         typeobj = this.type()
         typeobj = typeobj.obj()
         DO CASE
            CASE this.pyobject == PyNone.obj()
               retval = .NULL.
            CASE typeobj == PyBoolType.obj()
               retval = PyLong_AsLong(this.pyobject) > 0
            CASE typeobj == PyIntType.obj() OR typeobj == PyNotLongType.obj()
               retval = PyLong_AsLong(this.pyobject)
            CASE typeobj == PyFloatType.obj()
               retval = PyFloat_AsDouble(this.pyobject)
            CASE typeobj == PyBytesType.obj()
               LOCAL string_pointer, string_length
               string_length = PyBytes_Size(this.pyobject)
               string_pointer = PyBytes_AsString(this.pyobject)
               retval = SYS(2600, string_pointer, string_length)
            CASE typeobj == PyStrType.obj()
               retval = this.callmethod('encode', CREATEOBJECT('PythonTuple', 'utf-8'))
            CASE typeobj == PyDateType.obj()
               retval = DATE(this.getattr('year'), this.getattr('month'), this.getattr('day'))
            CASE typeobj == PyDatetimeType.obj()
               retval = DATETIME(this.getattr('year'), this.getattr('month'), this.getattr('day'),;
                                 this.getattr('hour'), this.getattr('minute'), this.getattr('second'))
            CASE typeobj == PyDecimalType.obj()
               retval = VAL(PyStrType.Call(CREATEOBJECT('pythontuple', this)))
            OTHERWISE
               retval = this
         ENDCASE
      ENDIF
      RETURN retval
   ENDPROC

   PROCEDURE GetItemRetObj(ind)
      LOCAL pyind, retval
      pyind = CREATEOBJECT('PythonObject', ind)
      retval = PyObject_GetItem(this.pyobject, pyind.obj())
      IF retval == 0
         ERROR py_error()
         RETURN .NULL.
      ENDIF
      retval = CREATEOBJECT('PythonObjectImpl', retval)
      RETURN retval
   ENDPROC

   PROCEDURE GetItem(ind)
      LOCAL retval
      retval = this.GetItemRetObj(ind)
      IF ISNULL(retval)
         RETURN retval
      ENDIF
      RETURN retval.getval()
   ENDPROC

   PROCEDURE SetItem(ind, foxval)
      LOCAL oerr, pyind, pyval
      TRY
         pyind = CREATEOBJECT('PythonObject', ind)
         pyval = CREATEOBJECT('PythonObject', foxval)
         IF PyObject_SetItem(this.pyobject, pyind.obj(), pyval.obj()) == -1
            ERROR PY_ERROR()
         ENDIF
      CATCH TO OERR

      ENDTRY

      IF VARTYPE(OERR) == 'O'
         ERROR OERR.MESSAGE
         RETURN .F.
      ENDIF

      RETURN .T.
   ENDPROC

   PROCEDURE DelItem(ind)
      LOCAL pyind
      pyind = CREATEOBJECT('PythonObject', ind)
      IF PyObject_DelItem(this.pyobject, pyind.obj()) == -1
         ERROR PY_ERROR()
         RETURN .F.
      ENDIF
      RETURN .T.
   ENDPROC

   FUNCTION GetAttrRetObj(attrname)
      LOCAL attrobj, retval
      attrobj = PyObject_GetAttrString(this.pyobject, attrname)
      IF attrobj == 0
          ERROR PY_ERROR()
          RETURN .NULL.
      ENDIF
      RETURN CREATEOBJECT('PythonObjectImpl', attrobj)
   ENDFUNC

   PROCEDURE GetAttr(attrname)
      LOCAL retval
      retval = this.GetAttrRetObj(attrname)
      IF TYPE('retval') != 'O'
          RETURN .NULL.
      ENDIF
      retval = retval.getval()
      RETURN retval
   ENDPROC

   PROCEDURE SetAttr(attrname, foxval)
      LOCAL retval
      pyval = CREATEOBJECT('PythonObject', foxval)
      IF PyObject_SetAttrString(this.pyobject, attrname, pyval.obj()) == -1
         ERROR PY_ERROR()
         RETURN .F.
      ENDIF
      RETURN .T.
   ENDPROC

   FUNCTION Iter_Access
      LOCAL retval, pyiter, nextitem
      retval = CREATEOBJECT("collection")
      pyiter = PyObject_GetIter(this.pyobject)
      IF pyiter == 0
         ERROR py_error()
         RETURN retval
      ENDIF
      nextitem = PyIter_Next(pyiter)
      DO WHILE nextitem != 0
         pyitem = CREATEOBJECT('PythonObjectImpl', nextitem)
         retval.add(pyitem.getval())
         nextitem = PyIter_Next(pyiter)
      ENDDO
      Py_DecRef(pyiter)
      RETURN retval
   ENDPROC

   PROCEDURE Repr
      LOCAL pyobject
      pyobject = CREATEOBJECT('PythonObjectImpl', PyObject_Repr(this.pyobject))
      RETURN pyobject.getval()
   ENDPROC

   FUNCTION Type()
      return CREATEOBJECT('PythonObjectImpl', PyObject_Type(this.pyobject))
   ENDFUNC

   PROCEDURE CallRetObj(argtuple, kwarg_dict)
      LOCAL pyobj, pyval
      DO CASE
      CASE VARTYPE(argtuple) != 'O'
         pyobj = PyObject_Call(this.pyobject, PyEmptyTuple.obj(), 0)
      CASE VARTYPE(kwarg_dict) != 'O'
         pyobj = PyObject_Call(this.pyobject, argtuple.obj(), 0)
      OTHERWISE
         pyobj = PyObject_Call(this.pyobject, argtuple.obj(), kwarg_dict.obj())
      ENDCASE

      IF pyobj == 0
         ERROR py_error()
         RETURN .F.
      ENDIF

      RETURN CREATEOBJECT('PythonObjectImpl', pyobj)
   ENDPROC

   PROCEDURE Call(argtuple, kwarg_dict)
      LOCAL pyval
      pyval = this.CallRetObj(argtuple, kwarg_dict)
      IF VARTYPE(pyval) == 'O'
         RETURN pyval.getval()
      ELSE
         RETURN .NULL.
      ENDIF
   ENDPROC

   PROCEDURE CallMethodRetObj(obj_method, argtuple, kwarg_dict)
      LOCAL funcobj
      funcobj = this.GetAttrRetObj(obj_method)
      IF VARTYPE(funcobj) != 'O'
          RETURN .F.
      ENDIF
      RETURN funcobj.CallRetObj(argtuple, kwarg_dict)
   ENDPROC

   PROCEDURE CallMethod(obj_method, argtuple, kwarg_dict)
      LOCAL funcobj
      funcobj = this.GetAttrRetObj(obj_method)
      IF VARTYPE(funcobj) != 'O'
          RETURN .F.
      ENDIF
      RETURN funcobj.call(argtuple, kwarg_dict)
   ENDPROC

   PROCEDURE DESTROY
      IF this.pyobject != 0 AND Py_IsInitialized() != 0
         Py_DecRef(this.pyobject)
      ENDIF
   ENDPROC

ENDDEFINE

DEFINE CLASS PythonDictionary AS PythonObjectImpl
   FUNCTION INIT(TUPLE_OF_2_PLES)
      this.pyobject = PyDict_New()
      IF VARTYPE(TUPLE_OF_2_PLES) == 'O'
         FOR EACH tuple IN tuple_of_2_ples.iter
            this.setitem(tuple.getitem(0), tuple.getitem(1))
         ENDFOR
      ENDIF
   ENDFUNC
ENDDEFINE

DEFINE CLASS PythonList AS PythonObjectImpl
   FUNCTION INIT(foxarray)
      this.pyobject = PyList_New(0)
      IF PARAMETERS() > 0
         IF TYPE('foxarray', 1) == 'A'
            LOCAL element, elemTuple
            FOR EACH element IN foxarray
               elemTuple = CREATEOBJECT('PythonTuple', element)
               this.CallMethod('append', elemTuple, .NULL.)
            ENDFOR
         ELSE
            ERROR 'input must be an array'
            RETURN .F.
         ENDIF
      ENDIF
   ENDFUNC
ENDDEFINE

DEFINE CLASS PythonModule AS PythonObjectImpl
   FUNCTION INIT(modulename)
      this.pyobject = PyImport_ImportModule(modulename)
      IF this.pyobject == 0
         ERROR py_error()
         RETURN .F.
      ENDIF
   ENDFUNC
ENDDEFINE

DEFINE CLASS PythonObject AS PythonObjectImpl
   FUNCTION INIT(foxval)
      LOCAL valtype, pyobject, oerr
      valtype = VARTYPE(foxval)
      DO CASE
         CASE valtype == 'N'
            IF ROUND(foxval, 0) == foxval
               pyobject = PyLong_FromLong(foxval)
            ELSE
               pyobject = PyFloat_FromDouble(foxval)
            ENDIF
         CASE valtype == 'C'
            pyobject = PyUnicode_FromStringAndSize(foxval, len(foxval))
         CASE valtype == 'L'
            pyobject = PyBool_FromLong(foxval)
         CASE valtype == 'O'
            TRY
               pyobject = foxval.obj()
            CATCH TO OERR

            ENDTRY

            IF VARTYPE(OERR) == 'O'
               ERROR 'Cannot create PythonObject from foxpro classes'
               RETURN
            ENDIF

            Py_IncRef(pyobject)
         CASE valtype == 'T' OR valtype == 'D'
            LOCAL DateTuple, DateMethod
            IF valtype == 'T'
               DateTuple = CREATEOBJECT('PythonTuple', YEAR(foxval), MONTH(foxval), DAY(foxval), HOUR(foxval), MINUTE(foxval), SEC(foxval))
               DateMethod = 'datetime'
            ELSE
               DateTuple = CREATEOBJECT('PythonTuple', YEAR(foxval), MONTH(foxval), DAY(foxval))
               DateMethod = 'date'
            ENDIF
            TRY
               pyobject = PyDatetime.CallMethodRetObj(DateMethod, DateTuple, .NULL.)
            CATCH TO OERR
               pyobject = PyNone
            ENDTRY
            Py_IncRef(pyobject.obj())
            pyobject = pyobject.obj()
         CASE valtype == 'X'
            pyobject = PyNone.obj()
            Py_IncRef(pyobject)
         OTHERWISE
            ERROR 'Unknown Variable Type/Cannot create pythonObject'
            RETURN
      ENDCASE
      this.pyobject = pyobject
   ENDFUNC
ENDDEFINE

DEFINE CLASS PythonTuple AS PythonObjectImpl
   FUNCTION INIT
      LPARAMETERS arg1,arg2,arg3,arg4,arg5, arg6,arg7,arg8,arg9

      LOCAL numparams, argnum

      numparams = MIN(PARAMETERS(), 9)

      this.pyobject = PyTuple_New(numparams)
      FOR argnum = 1 to numparams
         LOCAL pyval
         pyval = CREATEOBJECT('PythonObject', EVALUATE('arg' + STR(argnum,1)))
         Py_IncRef(pyval.obj())
         IF PyTuple_SetItem(this.pyobject, argnum-1, pyval.obj()) != 0
            RETURN .F.
         ENDIF
         RELEASE pyval
      ENDFOR
   ENDFUNC
ENDDEFINE

DEFINE CLASS PythonBuiltin AS PythonObjectImpl
   FUNCTION INIT(varname)
      LOCAL pyval
      pyval = PyBuiltins.GetAttrRetObj(varname)
      this.pyobject = pyval.obj()
   ENDFUNC
ENDDEFINE

FUNCTION PythonFunctionCall(modulename, funcname, argtuple, kwarg_dict)
   LOCAL pymod, oerr
   TRY
      pymod = CREATEOBJECT('PythonModule', modulename)
   CATCH TO OERR

   ENDTRY

   IF VARTYPE(OERR) == 'O'
      ERROR OERR.MESSAGE
      RETURN
   ENDIF

   RETURN pymod.CallMethod(funcname, argtuple, kwarg_dict)
ENDFUNC

FUNCTION py_error
   LOCAL err, pytype, pyvalue, pytraceback, errlist, pynewline
   err = PyErr_Occurred()

   IF err == 0
      RETURN ''
   ENDIF

   pytype = 0
   pyvalue = 0
   pytraceback = 0

   PyErr_Fetch(@pytype, @pyvalue, @pytraceback)
   PyErr_NormalizeException(@pytype, @pyvalue, @pytraceback)
   IF pytype != 0
      pytype = CREATEOBJECT('PythonObjectImpl', pytype)
      Py_IncRef(pytype.obj())
   ELSE
      pytype = PyNone
   ENDIF
   IF pyvalue != 0
      pyvalue = CREATEOBJECT('PythonObjectImpl', pyvalue)
   ELSE
      pyvalue = PyNone
   ENDIF
   IF pytraceback != 0
      pytraceback = CREATEOBJECT('PythonObjectImpl', pytraceback)
      Py_IncRef(pytraceback.obj())
   ELSE
      pytraceback = PyNone
   ENDIF

   local valuetuple, exc_info
   valuetuple = CREATEOBJECT('pythontuple', pyvalue)
   exc_info = CREATEOBJECT('pythontuple', pytype, pyvalue, pytraceback)
   pylogger.callmethod('error', valuetuple, CreateObject('PythonDictionary', CreateObject('PythonTuple', CreateObject('PythonTuple', 'exc_info', exc_info))))
   error_message = pytype.getattr('__name__') + ': ' + PyStrType.Call(valuetuple)
   return error_message
ENDFUNC

DEFINE CLASS PyStdoutRedirect AS CUSTOM
   io = .NULL.

   FUNCTION INIT(output_type)
      LOCAL stringiomodule
      IF PyMajorVersion == 2
         stringiomodule = 'StringIO'
      ELSE
         stringiomodule = 'io'
      ENDIF
      this.io = PythonFunctionCall(stringiomodule, 'StringIO', PyEmptyTuple)
      PySys.setAttr(output_type, this.io)
   ENDFUNC

   FUNCTION read
      retval = this.io.callmethod('getvalue', PyEmptyTuple)
      this.io.callmethod('seek', CreateObject('PythonTuple', 0))
      this.io.callmethod('truncate', PyEmptyTuple)
      RETURN retval
   ENDFUNC
ENDDEFINE

PROCEDURE GetMajorVersion(dllfile)
   Local Version, RegEx, Oerr
   LOCAL ARRAY DllInfo[1]

   Version = 0
   TRY
      AGETFILEVERSION(DllInfo, dllfile)
      Local RegEx
      RegEx = CreateObject('VBScript.RegExp')
      RegEx.pattern = '^([0-9]*)\.([0-9]*)\.([0-9]*)'
      IF RegEx.test(DllInfo(4))
         Version = INT(val(RegEx.replace(DllInfo[4], '$1')))
      ENDIF
   CATCH TO Oerr
   ENDTRY

   Return Version
ENDPROC

PROCEDURE FindDll(PythonHome)
   LOCAL TESTFILE
   TESTFILE = SYS(2000, ADDBS(PythonHome) + 'python??.dll')
   DO WHILE NOT ISBLANK(TESTFILE) AND LEN(TESTFILE) < LEN('python??.dll')
      TESTFILE = SYS(2000, ADDBS(PythonHome) + 'python??.dll', 1)
   ENDDO
   RETURN ADDBS(PythonHome) + TESTFILE
ENDPROC

PROCEDURE start_python(PythonHome, PythonDllArg, PythonExecutable)
   PUBLIC PythonRuntime
   LOCAL ERROR_OBJ
   If VARTYPE(PythonRuntime) != 'O'
      TRY
         DO CASE
         CASE PCOUNT() == 0
            PythonRuntime = CreateObject('PythonRuntime')
         CASE PCOUNT() == 1
            PythonRuntime = CreateObject('PythonRuntime', PythonHome)
         CASE PCOUNT() == 2
            PythonRuntime = CreateObject('PythonRuntime', PythonHome, PythonDllArg)
         CASE PCOUNT() == 3
            PythonRuntime = CreateObject('PythonRuntime', PythonHome, PythonDllArg, PythonExecutable)
         ENDCASE
      CATCH TO ERROR_OBJ
      ENDTRY
      If VARTYPE(PythonRuntime) != 'O'
         ERROR ERROR_OBJ.MESSAGE
      ENDIF
   ENDIF
ENDPROC

DEFINE CLASS PythonRuntime AS Custom
   OnlyOne = .F.
   PythonDll = .Null.

   PROCEDURE CHECK_REINIT
      LOCAL Instance_Index
      LOCAL ARRAY Instance_Array[1]
      RETURN AINSTANCE(Instance_Array, 'PythonRuntime') > 0
   ENDPROC

   PROCEDURE INIT(PythonHome, PythonDllArg, PythonExecutable)
      IF THIS.CHECK_REINIT()
         ERROR 'Python Already Initialized'
         RETURN
      ENDIF

      PUBLIC PythonDll, PyMajorVersion
      IF VARTYPE(PythonHome) != 'C'
         PythonHome = 'Python27'
      ENDIF
      IF PCOUNT() > 1
         PythonDll = PythonDllArg
      ELSE
         PythonDll = FindDll(PythonHome)
      ENDIF
      IF PCOUNT() > 2
      ELSE
         PythonExecutable = ADDBS(PythonHome) + 'pythonw.exe'
      ENDIF

      PyMajorVersion = GetMajorVersion(PythonDll)
      IF PyMajorVersion == 0
         ERROR 'Could not determine python version'
         RETURN
      ENDIF

      This.PythonDll = PythonDll

      DECLARE integer Py_IsInitialized IN (PythonDll)

      IF Py_IsInitialized() == 0
         LOCAL PythonHomeOriginal
         PythonHomeOriginal = PythonHome
         PythonHome = PythonHome + CHR(0)
         IF PyMajorVersion == 3
            PythonHome = STRCONV(PythonHome, 12)
         ENDIF

         DECLARE integer GetProcessHeap IN WIN32API
         DECLARE integer HeapAlloc IN WIN32API integer, integer, integer
         DECLARE integer Py_GetPythonHome IN (PythonDll)

         LOCAL PythonHomeMemory
         PythonHomeMemory = HeapAlloc(GetProcessHeap(), 0, LEN(PythonHome))
         SYS(2600, PythonHomeMemory, LEN(PythonHome), PythonHome)

         DECLARE Py_SetPythonHome IN (PythonDll) integer
         Py_SetPythonHome(PythonHomeMemory)


         LOCAL HomeCheck
         HomeCheck = SYS(2600, Py_GetPythonHome(), LEN(pythonhome))
         IF PyMajorVersion == 3
            HomeCheck = STRCONV(HomeCheck, 10)
         ENDIF
         IF LEFT(HomeCheck, LEN(PythonHomeOriginal)) != PythonHomeOriginal
            ERROR 'Memory Error setting python home'
            RETURN
         ENDIF

         DECLARE Py_InitializeEx IN (PythonDll) integer
         Py_InitializeEx(0)
         This.OnlyOne = .T.

         CLEAR DLLS GetProcessHeap, HeapAlloc, Py_SetPythonHome, Py_GetPythonHome, Py_Initialize

         DECLARE Py_IncRef IN (PythonDll) integer
         DECLARE Py_DecRef IN (PythonDll) integer
         DECLARE integer PyErr_Occurred IN (PythonDll)
         DECLARE PyErr_Fetch IN (PythonDll) integer @, integer @, integer @
         DECLARE PyErr_NormalizeException IN (PythonDll) integer @, integer @, integer @
         DECLARE integer PyImport_ImportModule IN (PythonDll) string
         IF PyMajorVersion == 2
            DECLARE integer PyUnicodeUCS2_FromStringAndSize IN (PythonDll) As PyUnicode_FromStringAndSize string, integer
            DECLARE integer PyString_AsString IN (PythonDll) AS PyBytes_AsString integer
            DECLARE integer PyString_Size IN (PythonDll) AS PyBytes_Size integer
            DECLARE integer PyString_FromStringAndSize IN (PythonDll) AS PyBytes_FromStringAndSize string, integer
         ELSE
            DECLARE integer PyUnicode_FromStringAndSize IN (PythonDll) string, integer
            DECLARE integer PyBytes_AsString IN (PythonDll) integer
            DECLARE integer PyBytes_Size IN (PythonDll) integer
            DECLARE integer PyBytes_FromStringAndSize IN (PythonDll) string, integer
         ENDIF
         DECLARE long PyLong_AsLong IN (PythonDll) integer
         DECLARE integer PyLong_FromLong IN (PythonDll) long
         DECLARE integer PyBool_FromLong IN (PythonDll) long
         DECLARE double PyFloat_AsDouble IN (PythonDll) integer
         DECLARE integer PyFloat_FromDouble IN (PythonDll) double
         DECLARE integer PyObject_Repr IN (PythonDll) integer
         DECLARE integer PyObject_GetAttrString IN (PythonDll) integer, string
         DECLARE integer PyObject_SetAttrString IN (PythonDll) integer, string, integer
         DECLARE integer PyObject_Type IN (PythonDll) integer
         DECLARE integer PyObject_Call IN (PythonDll) integer, integer, integer
         DECLARE integer PyObject_GetItem IN (PythonDll) integer, integer
         DECLARE integer PyObject_SetItem IN (PythonDll) integer, integer, integer
         DECLARE integer PyObject_DelItem IN (PythonDll) integer, integer
         DECLARE integer PyObject_GetIter IN (PythonDll) integer
         DECLARE integer PyIter_Next IN (PythonDll) integer
         DECLARE integer PyDict_New IN (PythonDll)
         DECLARE integer PyList_New IN (PythonDll) integer
         DECLARE integer PyTuple_New IN (PythonDll) integer
         DECLARE integer PyTuple_SetItem IN (PythonDll) integer, integer, integer

         PUBLIC PyBuiltins, PyNone, PyDatetime, PySys, PyStderr, PyStdout, PyLogger, PyEmptyTuple
         PUBLIC PyStrType, PyBytesType, PyBoolType, PyIntType, PyNotLongType, PyFloatType, PyDatetimeType, PyDateType, PyDecimalType
         PyEmptyTuple = CREATEOBJECT('PythonTuple')
         IF PyMajorVersion == 2
            PyBuiltins = CREATEOBJECT('PythonModule', '__builtin__')
         ELSE
            PyBuiltins = CREATEOBJECT('PythonModule', 'builtins')
         ENDIF
         PyNone = PyBuiltins.GetAttrRetObj('None')
         PyDatetime = CREATEOBJECT('PythonModule', 'datetime')
         PySys = CREATEOBJECT('PythonModule', 'sys')
         IF PyMajorVersion == 2
            PyStrType = PyBuiltins.GetAttrRetObj('unicode')
            PyNotLongType = PyBuiltins.GetAttrRetObj('int')
            PyIntType = PyBuiltins.GetAttrRetObj('long')
         ELSE
            PyStrType = PyBuiltins.GetAttrRetObj('str')
            PyIntType = PyBuiltins.GetAttrRetObj('int')
            PyNotLongType = PyBuiltins.GetAttrRetObj('int')
         ENDIF
         PyBytesType = PyBuiltins.GetAttrRetObj('bytes')
         PyBoolType = PyBuiltins.GetAttrRetObj('bool')
         PyFloatType = PyBuiltins.GetAttrRetObj('float')
         PyDatetimeType = PyDatetime.GetAttrRetObj('datetime')
         PyDateType = PyDatetime.GetAttrRetObj('date')
         Local Decimal_Module
         Decimal_Module = CREATEOBJECT('PythonModule', 'decimal')
         PyDecimalType = Decimal_Module.GetAttrRetObj('Decimal')
         PySysPath = PySys.getAttrRetObj('path')
         PySysPath.CallMethod('append', CREATEOBJECT('PythonTuple', CURDIR()), .NULL.)
         PySys.setAttr('executable', PythonExecutable)
         PySysArgv = CREATEOBJECT('PythonList')
         PySysArgv.callmethod('append', CREATEOBJECT('PythonTuple', ''), .NULL.)
         PySys.setAttr('argv', PySysArgv)
         PyStdout = CREATEOBJECT('PyStdoutRedirect', 'stdout')
         PyStderr = CREATEOBJECT('PyStdoutRedirect', 'stderr')
         PyLogger = PythonFunctionCall('logging', 'getLogger', CREATEOBJECT('pythontuple', 'foxpro2python'))
      ENDIF
   ENDPROC

   PROCEDURE DestroyPythonInstances(ClassType)
      LOCAL Instance_Index
      LOCAL ARRAY Instance_Array[1]
      FOR Instance_Index = 1 TO AINSTANCE(Instance_Array, ClassType)
         RELEASE (Instance_Array[Instance_Index])
      ENDFOR
   ENDPROC

   PROCEDURE Destroy
      IF NOT THIS.OnlyOne
         RETURN
      ENDIF
      RELEASE PyStdOut, PyStdErr
      RELEASE PythonDll, PyMajorVersion

      DECLARE integer Py_IsInitialized IN (This.PythonDll)
      DECLARE Py_DecRef IN (This.PythonDll) integer
      DECLARE Py_Finalize IN (This.PythonDll)

      THIS.DestroyPythonInstances('PythonBuiltin')
      THIS.DestroyPythonInstances('PythonTuple')
      THIS.DestroyPythonInstances('PythonObject')
      THIS.DestroyPythonInstances('PythonModule')
      THIS.DestroyPythonInstances('PythonList')
      THIS.DestroyPythonInstances('PythonDictionary')
      THIS.DestroyPythonInstances('PythonObjectImpl')

      Py_Finalize()

      DECLARE integer GetProcessHeap IN WIN32API
      DECLARE integer HeapFree IN WIN32API integer, integer, integer
      DECLARE integer Py_GetPythonHome IN (This.PythonDll)
      DECLARE Py_SetPythonHome IN (This.PythonDll) integer

      HeapFree(GetProcessHeap(), 0, Py_GetPythonHome())

      Py_SetPythonHome(0)

      CLEAR DLLS GetProcessHeap, HeapAlloc, Py_SetPythonHome, Py_GetPythonHome, Py_Initialize

      FOR Instance_Index = 1 TO ADLLS(Instance_Array)
         IF LOWER(Instance_Array[Instance_Index, 3]) == LOWER(This.PythonDll)
            LOCAL DLLMETHOD
            DLLMETHOD = Instance_Array[Instance_Index, 2]
            CLEAR DLLS &DLLMETHOD
         ENDIF
      ENDFOR
   ENDPROC
ENDDEFINE
