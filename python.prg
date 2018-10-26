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
               retval = PyInt_AsLong(this.pyobject) > 0
            CASE typeobj == PyIntType.obj()
               retval = PyInt_AsLong(this.pyobject)
            CASE typeobj == PyFloatType.obj()
               retval = PyFloat_AsDouble(this.pyobject)
            CASE typeobj == PyStrType.obj()
               LOCAL string_pointer, string_length
               string_length = PyString_Size(this.pyobject)
               string_pointer = PyString_AsString(this.pyobject)
               retval = SYS(2600, string_pointer, string_length)
            CASE typeobj == PyUnicodeType.obj()
               retval = this.callmethod('encode', CREATEOBJECT('pythontuple', 'utf-8'))
            CASE typeobj == PyDateType.obj()
               retval = DATE(this.getattr('year'), this.getattr('month'), this.getattr('day'))
            CASE typeobj == PyDatetimeType.obj()
               retval = DATETIME(this.getattr('year'), this.getattr('month'), this.getattr('day'),;
                                 this.getattr('hour'), this.getattr('minute'), this.getattr('second'))
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
      funcobj = this.GetAttr(obj_method)
      IF VARTYPE(funcobj) != 'O'
          RETURN .F.
      ENDIF
      RETURN funcobj.CallRetObj(argtuple, kwarg_dict)
   ENDPROC

   PROCEDURE CallMethod(obj_method, argtuple, kwarg_dict)
      LOCAL funcobj
      funcobj = this.GetAttr(obj_method)
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
               pyobject = PyInt_FromLong(foxval)
            ELSE
               pyobject = PyFloat_FromDouble(foxval)
            ENDIF
         CASE valtype == 'C'
            pyobject = PyString_FromStringAndSize(foxval, len(foxval))
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
            pyobject = PyDatetime.CallMethodRetObj(DateMethod, DateTuple, .NULL.)
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

DEFINE CLASS PythonVariable AS PythonObjectImpl
   FUNCTION INIT(varname)
      LOCAL main_mod, pyval
      main_mod = CREATEOBJECT('PythonModule', '__main__')
      pyval = main_mod.GetAttrRetObj(varname)
      this.pyobject = pyval.obj()
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
   outtype = ''

   FUNCTION INIT(output_type)
      this.outtype = output_type
      this.reset_io()
   ENDFUNC

   FUNCTION reset_io
      this.io = PythonFunctionCall('StringIO', 'StringIO', PyEmptyTuple)
      PySys.setAttr(this.outtype, this.io)
   ENDFUNC

   FUNCTION read
      LOCAL retval
      retval = this.io.callmethod('getvalue', PyEmptyTuple)
      this.reset_io()
      RETURN retval
   ENDFUNC
ENDDEFINE

PROCEDURE start_python
   DECLARE integer Py_IsInitialized IN Python27\python27.dll

   IF Py_IsInitialized() == 0
      DECLARE Py_SetPythonHome IN Python27\python27.dll string
      DECLARE Py_Initialize IN Python27\python27.dll
      Py_SetPythonHome('Python27')
      Py_Initialize()
      CLEAR DLLS Py_SetPythonHome, Py_Initialize

      DECLARE Py_IncRef IN Python27\python27.dll integer
      DECLARE Py_DecRef IN Python27\python27.dll integer
      DECLARE integer PyErr_Occurred IN Python27\python27.dll
      DECLARE PyErr_Fetch IN Python27\python27.dll integer @, integer @, integer @
      DECLARE PyErr_NormalizeException IN Python27\python27.dll integer @, integer @, integer @
      DECLARE integer PyImport_ImportModule IN Python27\python27.dll string
      DECLARE integer PyString_AsString IN Python27\python27.dll integer
      DECLARE integer PyString_Size IN Python27\python27.dll integer
      DECLARE integer PyString_FromStringAndSize IN Python27\python27.dll string, integer
      DECLARE long PyInt_AsLong IN Python27\python27.dll integer
      DECLARE integer PyInt_FromLong IN Python27\python27.dll long
      DECLARE integer PyBool_FromLong IN Python27\python27.dll long
      DECLARE double PyFloat_AsDouble IN Python27\python27.dll integer
      DECLARE integer PyFloat_FromDouble IN Python27\python27.dll double
      DECLARE integer PyObject_Repr IN Python27\python27.dll integer
      DECLARE integer PyObject_GetAttrString IN Python27\python27.dll integer, string
      DECLARE integer PyObject_SetAttrString IN Python27\python27.dll integer, string, integer
      DECLARE integer PyObject_Type IN Python27\python27.dll integer
      DECLARE integer PyObject_Call IN Python27\python27.dll integer, integer, integer
      DECLARE integer PyObject_GetItem IN Python27\python27.dll integer, integer
      DECLARE integer PyObject_SetItem IN Python27\python27.dll integer, integer, integer
      DECLARE integer PyObject_DelItem IN Python27\python27.dll integer, integer
      DECLARE integer PyObject_GetIter IN Python27\python27.dll integer
      DECLARE integer PyIter_Next IN Python27\python27.dll integer
      DECLARE integer PyDict_New IN Python27\python27.dll
      DECLARE integer PyList_New IN Python27\python27.dll integer
      DECLARE integer PyTuple_New IN Python27\python27.dll integer
      DECLARE integer PyTuple_SetItem IN Python27\python27.dll integer, integer, integer

      PUBLIC PyBuiltins, PyNone, PyDatetime, PySys, PyStderr, PyStdout, PyLogger, PyEmptyTuple
      PUBLIC PyUnicodeType, PyStrType, PyBoolType, PyIntType, PyFloatType, PyDatetimeType, PyDateType
      PyEmptyTuple = CREATEOBJECT('PythonTuple')
      PyBuiltins = CREATEOBJECT('PythonModule', '__builtin__')
      PyNone = PyBuiltins.GetAttrRetObj('None')
      PyDatetime = CREATEOBJECT('PythonModule', 'datetime')
      PySys = CREATEOBJECT('PythonModule', 'sys')
      PyUnicodeType = PyBuiltins.GetAttrRetObj('unicode')
      PyStrType = PyBuiltins.GetAttrRetObj('str')
      PyBoolType = PyBuiltins.GetAttrRetObj('bool')
      PyIntType = PyBuiltins.GetAttrRetObj('int')
      PyFloatType = PyBuiltins.GetAttrRetObj('float')
      PyDatetimeType = PyDatetime.GetAttrRetObj('datetime')
      PyDateType = PyDatetime.GetAttrRetObj('date')
      PySysPath = PySys.getAttrRetObj('path')
      PySysPath.CallMethod('append', CREATEOBJECT('PythonTuple', CURDIR()), .NULL.)
      PySys.setAttr('executable', CURDIR() + 'Python27\pythonw.exe')
      PySysArgv = CREATEOBJECT('PythonList')
      PySysArgv.callmethod('append', CREATEOBJECT('PythonTuple', ''), .NULL.)
      PySys.setAttr('argv', PySysArgv)
      PyStdout = CREATEOBJECT('PyStdoutRedirect', 'stdout')
      PyStderr = CREATEOBJECT('PyStdoutRedirect', 'stderr')
      PyLogger = PythonFunctionCall('logging', 'getLogger', CREATEOBJECT('pythontuple', 'foxpro2python'))
   ENDIF
ENDPROC

PROCEDURE stop_python
   RELEASE PyBuiltins, PyNone, PyDatetime, PySys, PyStderr, PyStdout, PyLogger, PyEmptyTuple
   RELEASE PyUnicodeType, PyStrType, PyBoolType, PyIntType, PyFloatType, PyDatetimeType, PyDateType
   DECLARE integer Py_IsInitialized IN Python27\python27.dll
   IF Py_IsInitialized() != 0
      DECLARE Py_Finalize IN Python27\python27.dll
      Py_Finalize()
      CLEAR DLLS Py_Finalize
   ENDIF
ENDPROC
