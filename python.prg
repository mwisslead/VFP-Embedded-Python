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
      LOCAL retval, typestr
      IF this.pyobject > 0
         typestr = this.type()
         DO CASE
            CASE typestr == "<type 'bool'>"
               retval = PyInt_AsLong(this.pyobject) > 0
            CASE typestr == "<type 'int'>"
               retval = PyInt_AsLong(this.pyobject)
            CASE typestr == "<type 'float'>"
               retval = PyFloat_AsDouble(this.pyobject)
            CASE typestr == "<type 'str'>"
               retval = PyString_AsString(this.pyobject)
            CASE typestr == "<type 'unicode'>"
               retval = PyString_AsString(this.pyobject)
            CASE typestr == "<type 'datetime.date'>"
               retval = DATE(this.getattr('year'), this.getattr('month'), this.getattr('day'))
            CASE typestr == "<type 'datetime.datetime'>"
               year_val = this.getattr('year')
               month_val = this.getattr('month')
               retval = DATETIME(this.getattr('year'), this.getattr('month'), this.getattr('day'),;
                                 this.getattr('hour'), this.getattr('minute'), this.getattr('second'))
            CASE typestr == "<type 'NoneType'>"
               retval = .NULL.
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
      LOCAL pyind, pyval
      pyind = CREATEOBJECT('PythonObject', ind)
      pyval = CREATEOBJECT('PythonObject', foxval)
      RETURN PyObject_SetItem(this.pyobject, pyind.obj(), pyval.obj()) != -1
   ENDPROC

   PROCEDURE DelItem(ind)
      LOCAL pyind
      pyind = CREATEOBJECT('PythonObject', ind)
      RETURN PyObject_DelItem(this.pyobject, pyind.obj()) != -1
   ENDPROC

   FUNCTION GetAttrRetObj(attrname)
      LOCAL attrobj, retval
      attrobj = PyObject_GetAttrString(this.pyobject, attrname)
      IF attrobj == 0
          ERROR('object does not contain attribute: ' + attrname)
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
      pyval = CREATEOBJECT('PythonObject', foxval)
      RETURN PyObject_SetAttrString(this.pyobject, attrname, pyval.obj()) != -1
   ENDPROC

   FUNCTION Iter_Access
      retval = CREATEOBJECT("collection")
      pyiter = PyObject_GetIter(this.pyobject)
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
      LOCAL pyrepr, retval
      pyrepr = PyObject_Repr(this.pyobject)
      retval = PyString_AsString(pyrepr)
      Py_DecRef(pyrepr)
      RETURN retval
   ENDPROC

   FUNCTION Type()
      LOCAL pyobjtype
      pyobjtype = CREATEOBJECT('PythonObjectImpl', PyObject_Type(this.pyobject))
      return pyobjtype.repr()
   ENDFUNC

   PROCEDURE CallRetObj(argtuple)
      LOCAL pyobj, pyval
      IF PyCallable_Check(this.pyobject) == 0
         ERROR 'Object not callable'
         RETURN .F.
      ENDIF

      IF VARTYPE(argtuple) <> 'O' OR argtuple.type() <> "<type 'tuple'>"
         ERROR 'Argument must be Python Tuple'
         RETURN .F.
      ENDIF

      pyobj = PyObject_CallObject(this.pyobject, argtuple.obj())

      IF pyobj == 0
         ERROR 'Function call failed: ' + py_error()
         RETURN .F.
      ENDIF

      RETURN CREATEOBJECT('PythonObjectImpl', pyobj)
   ENDPROC

   PROCEDURE Call(argtuple)
      LOCAL pyval
      pyval = this.CallRetObj(argtuple)
      IF VARTYPE(pyval) == 'O'
         RETURN pyval.getval()
      ELSE
         RETURN .NULL.
      ENDIF
   ENDPROC

   PROCEDURE CallMethodRetObj(obj_method, argtuple)
      LOCAL funcobj
      funcobj = this.GetAttr(obj_method)
      IF VARTYPE(funcobj) != 'O'
          RETURN .F.
      ENDIF
      RETURN funcobj.CallRetObj(argtuple)
   ENDPROC

   PROCEDURE CallMethod(obj_method, argtuple)
      LOCAL funcobj
      funcobj = this.GetAttr(obj_method)
      IF VARTYPE(funcobj) != 'O'
          RETURN .F.
      ENDIF
      RETURN funcobj.call(argtuple)
   ENDPROC

   PROCEDURE DESTROY
      IF this.pyobject != 0 AND Py_IsInitialized() != 0
         Py_DecRef(this.pyobject)
      ENDIF
   ENDPROC

ENDDEFINE

DEFINE CLASS PythonDictionary AS PythonObjectImpl
   FUNCTION INIT
      this.pyobject = PyDict_New()
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
               this.CallMethod('append', elemTuple)
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
         ERROR('Could not import module: ' + modulename + CHR(10) + py_error())
         RETURN .F.
      ENDIF
   ENDFUNC
ENDDEFINE

DEFINE CLASS PythonObject AS PythonObjectImpl
   FUNCTION INIT(foxval)
      LOCAL valtype, pyobject
      valtype = VARTYPE(foxval)
      DO CASE
         CASE valtype == 'N'
            IF ROUND(foxval, 0) == foxval
               pyobject = PyInt_FromLong(foxval)
            ELSE
               pyobject = PyFloat_FromDouble(foxval)
            ENDIF
         CASE valtype == 'C'
            pyobject = PyString_FromString(foxval)
         CASE valtype == 'L'
            pyobject = PyBool_FromLong(foxval)
         CASE valtype == 'O'
            IF PEMSTATUS(foxval, 'obj', 3) != 'Method'
               ERROR 'Cannot create PythonObject from foxpro class'
               RETURN
            ENDIF
            pyobject = foxval.obj()
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
            pyobject = PyDatetime.CallMethodRetObj(DateMethod, DateTuple)
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

FUNCTION PythonFunctionCall(modulename, funcname, argtuple)
   LOCAL pymod
   pymod = CREATEOBJECT('PythonModule', modulename)
   IF VARTYPE(pymod) == 'O' AND UPPER(argtuple.class) == 'PYTHONTUPLE'
      RETURN pymod.CallMethod(funcname, argtuple)
   ELSE
      ERROR('argtuple argument must be of type PythonTuple')
   ENDIF
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

   errlist = PythonFunctionCall('traceback', "format_exception", CREATEOBJECT('PythonTuple', pytype, pyvalue, pytraceback))
   RETURN errlist.getitem(-1)
ENDFUNC

DEFINE CLASS PyStdoutRedirect AS CUSTOM
   io = .NULL.
   outtype = ''

   FUNCTION INIT(output_type)
      this.outtype = output_type
      this.reset_io()
   ENDFUNC

   FUNCTION reset_io
      this.io = PythonFunctionCall('StringIO', 'StringIO', CREATEOBJECT('PythonTuple'))
      PySys.setAttr(this.outtype, this.io)
   ENDFUNC

   FUNCTION read
      LOCAL retval
      retval = this.io.callmethod('getvalue', CREATEOBJECT('PythonTuple'))
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
      CLEAR DLLS Py_SetPythonHome, PyInitialize

      DECLARE Py_IncRef IN Python27\python27.dll integer
      DECLARE Py_DecRef IN Python27\python27.dll integer
      DECLARE integer PyErr_Occurred IN Python27\python27.dll
      DECLARE PyErr_Fetch IN Python27\python27.dll integer @, integer @, integer @
      DECLARE integer PyImport_ImportModule IN Python27\python27.dll string
      DECLARE string PyString_AsString IN Python27\python27.dll integer
      DECLARE integer PyString_FromString IN Python27\python27.dll string
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
      DECLARE integer PyObject_CallObject IN Python27\python27.dll integer, integer
      DECLARE integer PyObject_GetItem IN Python27\python27.dll integer, integer
      DECLARE integer PyObject_SetItem IN Python27\python27.dll integer, integer, integer
      DECLARE integer PyObject_DelItem IN Python27\python27.dll integer, integer
      DECLARE integer PyObject_GetIter IN Python27\python27.dll integer
      DECLARE integer PyIter_Next IN Python27\python27.dll integer
      DECLARE integer PyCallable_Check IN Python27\python27.dll integer
      DECLARE integer PyDict_New IN Python27\python27.dll
      DECLARE integer PyList_New IN Python27\python27.dll integer
      DECLARE integer PyTuple_New IN Python27\python27.dll integer
      DECLARE integer PyTuple_SetItem IN Python27\python27.dll integer, integer, integer

      PUBLIC PyBuiltins, PyNone, PyDatetime, PySys, PyStderr, PyStdout
      PyBuiltins = CREATEOBJECT('PythonModule', '__builtin__')
      PyNone = PyBuiltins.GetAttrRetObj('None')
      PyDatetime = CREATEOBJECT('PythonModule', 'datetime')
      PySys = CREATEOBJECT('PythonModule', 'sys')
      PySysPath = PySys.getAttrRetObj('path')
      PySysPath.CallMethod('append', CREATEOBJECT('PythonTuple', CURDIR()))
      PySys.setAttr('executable', CURDIR() + 'Python27\pythonw.exe')
      PyStdout = CREATEOBJECT('PyStdoutRedirect', 'stdout')
      PyStderr = CREATEOBJECT('PyStdoutRedirect', 'stderr')
      RELEASE PySysPath
   ENDIF
ENDPROC

PROCEDURE stop_python
   RELEASE PyDatetime, PyNone, PyBuiltins, PySys, PyStderr, PyStdout
   DECLARE integer Py_IsInitialized IN Python27\python27.dll
   IF Py_IsInitialized() != 0
      DECLARE Py_Finalize IN Python27\python27.dll
      Py_Finalize()
      CLEAR DLLS PyFinalize
   ENDIF
ENDPROC
