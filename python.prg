DEFINE CLASS PythonObjectImpl AS Custom
   PROTECTED pyobject
   iter = .NULL.
   
   PROCEDURE INIT(pyobj) &&COMMENT
      IF pyobj == 0
         ERROR 'invalid input value'
         RETURN .F.
      ENDIF
      this.pyobject = pyobj
      Py_IncRef(pyobj)
   ENDPROC
   
   PROCEDURE obj
      RETURN this.pyobject
   ENDPROC

   PROCEDURE GETVAL
      LOCAL retval, pyval
      pyval = this.pyobject
      retval = NULL
      IF pyval > 0
         DO CASE
            CASE this.type() == "<type 'float'>"
               retval = PyFloat_AsDouble(pyval)
            CASE this.type() == "<type 'int'>"
               retval = PyInt_AsLong(pyval)
            CASE this.type() == "<type 'bool'>"
               retval = PyInt_AsLong(pyval) > 0
            CASE this.type() == "<type 'str'>"
               retval = PyString_AsString(pyval)
            CASE this.type() == "<type 'unicode'>"
               retval = PyString_AsString(pyval)
            CASE this.type() == "<type 'dict'>"
               retval = this
            CASE this.type() == "<type 'list'>"
               retval = this
            CASE this.type() == "<type 'datetime.date'>"
               retval = DATE(this.getattr('year'), this.getattr('month'), this.getattr('day'))
            CASE this.type() == "<type 'datetime.datetime'>"
               retval = DATETIME(this.getattr('year'), this.getattr('month'), this.getattr('day'),;
                                 this.getattr('hour'), this.getattr('minute'), this.getattr('second'))
            CASE this.type() == "<type 'NoneType'>"
               retval = .NULL.
            OTHERWISE
               retval = this
         ENDCASE
      ENDIF
      RETURN retval
   ENDPROC
  
   PROCEDURE GetItemRetObj(ind)
      pyind = CREATEOBJECT('PythonObject', ind)
      retval = CREATEOBJECT('PythonObjectImpl', PyObject_GetItem(this.pyobject, pyind.obj()))
      Py_DecRef(retval.obj())
      RETURN retval
   ENDPROC
  
   PROCEDURE GetItem(ind)
      retval = this.GetItemRetObj(ind)
      RETURN retval.getval()
   ENDPROC
  
   PROCEDURE SetItem(ind, foxval)
      pyind = CREATEOBJECT('PythonObject', ind)
      pyval = CREATEOBJECT('PythonObject', foxval)
      RETURN PyObject_SetItem(this.pyobject, pyind.obj(), pyval.obj()) != -1
   ENDPROC
   
   PROCEDURE DelItem(ind)
      pyind = CREATEOBJECT('PythonObject', ind)
      RETURN PyObject_DelItem(this.pyobject, pyind.obj()) != -1
   ENDPROC

   FUNCTION GetAttrRetObj(attrname)
      retval = CREATEOBJECT('PythonObjectImpl', PyObject_GetAttrString(this.pyobject, attrname))
      Py_DecRef(retval.obj())
      RETURN retval
   ENDFUNC
   
   PROCEDURE GetAttr(attrname)
      retval = this.GetAttrRetObj(attrname)
      RETURN retval.getval()
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
         Py_DecRef(nextitem)
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
      pyobjtype = CREATEOBJECT('PythonObjectImpl', PyObject_Type(this.pyobject))
      Py_DecRef(pyobjtype.obj())
      return pyobjtype.repr()
   ENDFUNC

   PROCEDURE CallRetObj(argtuple)

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

      pyval = CREATEOBJECT('PythonObjectImpl', pyobj)
      Py_DecRef(pyobj)
      RETURN pyval
   ENDPROC
   
   PROCEDURE Call(argtuple)
      pyval = this.CallRetObj(argtuple)
      RETURN pyval.getval()
   ENDPROC
   
   PROCEDURE CallMethodRetObj(obj_method, argtuple)
      funcobj = this.GetAttr(obj_method)
      RETURN funcobj.CallRetObj(argtuple)
   ENDPROC
   
   PROCEDURE CallMethod(obj_method, argtuple)
      funcobj = this.GetAttr(obj_method)
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
   FUNCTION init(foxarray)
      this.pyobject = PyList_New(0)
      IF TYPE('foxarray', 1) == 'A'
         FOR EACH element IN foxarray
            elemTuple = CREATEOBJECT('PythonTuple', element)
            this.CallMethod('append', elemTuple)
         ENDFOR
      ELSE
         ERROR 'input must be an array'
      ENDIF
   ENDFUNC
ENDDEFINE

DEFINE CLASS PythonModule AS PythonObjectImpl
   FUNCTION INIT(modulename)
      modname = CREATEOBJECT('PythonObject', modulename)
      this.pyobject = PyImport_Import(modname.obj())
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
         CASE valtype == 'T'
            DatetimeTuple = CREATEOBJECT('PythonTuple', YEAR(foxval), MONTH(foxval), DAY(foxval), HOUR(foxval), MINUTE(foxval), SEC(foxval))
            pyobject = PyDatetime.CallMethodRetObj('datetime', DatetimeTuple)
            pyobject = pyobject.obj()
         CASE valtype == 'D'
            DateTuple = CREATEOBJECT('PythonTuple', YEAR(foxval), MONTH(foxval), DAY(foxval))
            pyobject = PyDatetime.CallMethodRetObj('date', DateTuple)
            pyobject = pyobject.obj()
         CASE valtype == 'X'
            pyobject = PyNone
            pyobject = pyobject.obj()
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
      
      LOCAL numparams, retval
      
      numparams = PARAMETERS()
      
      IF numparams > 9
         numparams = 9
      ENDIF
      
      args = PyTuple_New(numparams)
      FOR n = 1 to numparams
         argpyval = EVALUATE("CREATEOBJECT('PythonObject', arg" + STR(n,1) + ')')
         Py_IncRef(argpyval.obj())
         PyTuple_SetItem(args, n-1, argpyval.obj())
         RELEASE argpyval
      ENDFOR
      
      this.pyobject = args
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
   RETURN pymod.CallMethod(funcname, argtuple)
ENDFUNC

FUNCTION py_error
   err = PyErr_Occurred()
    
   IF err == 0
      RETURN ''
   ENDIF

   ptype = 0
   pvalue = 0
   ptraceback = 0

   PyErr_Fetch(@ptype, @pvalue, @ptraceback)
    
   pytype = CREATEOBJECT('PythonObjectImpl', ptype)
   pyvalue = CREATEOBJECT('PythonObjectImpl', pvalue)
   pytraceback = CREATEOBJECT('PythonObjectImpl', ptraceback)
   Py_DecRef(pvalue)
   
   errlist = PythonFunctionCall('traceback', "format_exception", CREATEOBJECT('PythonTuple', pytype, pyvalue, pytraceback))
   pynewline = PyString_FromString(CHR(10))
   ret = _PyString_Join(pynewline, errlist.obj())
   Py_DECREF(pynewline)
   
   pyval = CREATEOBJECT('PythonObjectImpl', ret)
    
   RETURN pyval.getval()

ENDFUNC


PROCEDURE start_python
   DECLARE Py_SetPythonHome IN Python27\python27.dll string
   DECLARE string Py_GetPythonHome IN Python27\python27.dll
   DECLARE string Py_GetPath IN Python27\python27.dll
   DECLARE integer Py_Initialize IN Python27\python27.dll
   DECLARE integer Py_IsInitialized IN Python27\python27.dll
   DECLARE integer Py_Finalize IN Python27\python27.dll
   DECLARE Py_IncRef IN Python27\python27.dll integer
   DECLARE Py_DecRef IN Python27\python27.dll integer
   DECLARE integer PyErr_Occurred IN Python27\python27.dll
   DECLARE  PyErr_Fetch IN Python27\python27.dll integer @, integer @, integer @
   DECLARE integer PyImport_AddModule IN Python27\python27.dll string
   DECLARE integer PyImport_Import IN Python27\python27.dll integer
   DECLARE integer PySys_GetObject IN Python27\python27.dll string
   DECLARE integer PyRun_SimpleString IN Python27\python27.dll string
   DECLARE string PyString_AsString IN Python27\python27.dll integer
   DECLARE integer PyString_FromString IN Python27\python27.dll string
   DECLARE integer _PyString_Join IN Python27\python27.dll integer, integer
   DECLARE integer PyUnicode_FromString IN Python27\python27.dll string
   DECLARE integer _PyUnicode_Join IN Python27\python27.dll integer, integer
   DECLARE long PyInt_AsLong IN Python27\python27.dll integer
   DECLARE integer PyInt_FromLong IN Python27\python27.dll long
   DECLARE double PyFloat_AsDouble IN Python27\python27.dll integer
   DECLARE integer PyFloat_FromDouble IN Python27\python27.dll double
   DECLARE integer PyBool_FromLong IN Python27\python27.dll long
   DECLARE integer PyObject_Length IN Python27\python27.dll integer
   DECLARE integer PyObject_Str IN Python27\python27.dll integer
   DECLARE integer PyObject_Repr IN Python27\python27.dll integer
   DECLARE integer PyObject_GetAttrString IN Python27\python27.dll integer, string
   DECLARE integer PyObject_SetAttrString IN Python27\python27.dll integer, string, integer
   DECLARE integer PyObject_IsInstance IN Python27\python27.dll integer, integer
   DECLARE integer PyObject_Type IN Python27\python27.dll integer
   DECLARE integer PyObject_Call IN Python27\python27.dll integer, integer, integer
   DECLARE integer PyObject_CallObject IN Python27\python27.dll integer, integer
   DECLARE integer PyObject_GetItem IN Python27\python27.dll integer, integer
   DECLARE integer PyObject_SetItem IN Python27\python27.dll integer, integer, integer
   DECLARE integer PyObject_DelItem IN Python27\python27.dll integer, integer
   DECLARE integer PyObject_GetIter IN Python27\python27.dll integer
   DECLARE integer PyIter_Next IN Python27\python27.dll integer
   DECLARE integer PyCallable_Check IN Python27\python27.dll integer
   DECLARE integer PyEval_GetLocals IN Python27\python27.dll
   DECLARE integer PyEval_GetGlobals IN Python27\python27.dll
   DECLARE integer PyDict_New IN Python27\python27.dll
   DECLARE integer PyDict_GetItem IN Python27\python27.dll integer, integer
   DECLARE integer PyDict_SetItem IN Python27\python27.dll integer, integer, integer
   DECLARE integer PyDict_Keys IN Python27\python27.dll integer
   DECLARE integer PyList_New IN Python27\python27.dll integer
   DECLARE integer PyList_Append IN Python27\python27.dll integer, integer
   DECLARE integer PyList_Size IN Python27\python27.dll integer
   DECLARE integer PyList_GetItem IN Python27\python27.dll integer, integer
   DECLARE integer PyTuple_New IN Python27\python27.dll integer
   DECLARE integer PyTuple_SetItem IN Python27\python27.dll integer, integer, integer

   Py_SetPythonHome('Python27')
   Py_Initialize()
   PUBLIC PyBuiltins, PyNone, PyDatetime
   PyBuiltins = CREATEOBJECT('PythonVariable', '__builtins__')
   PyNone = PyBuiltins.GetAttrRetObj('None')
   PyDatetime = CREATEOBJECT('PythonModule', 'datetime')
   sysPath = PySys_GetObject('path')
   programName = CREATEOBJECT('PythonObject', CURDIR())
   PyList_Append(sysPath, programName.obj())
   RELEASE programName
ENDPROC

PROCEDURE stop_python
   RELEASE PyBuiltins, PyNone, PyDatetime
   Py_Finalize()
ENDPROC
