# foxpro2python bridge

Call python code directly from foxpro.

foxpro2python is a set of foxpro classes and functions that allow simple execution of python code from within a foxpro program.

Please see the example.prg for some usage examples.

## Features
- Automatically converts between basic data types.
- Errors raised in python are automatically raised as foxpro errors.

## To use
- Add python.prg to your project.
- Install the Python27 folder into the working directory of the foxpro program.
- Call the start_python procedure in python.prg
- Call python functions with the PythonFunctionCall function (e.g. PythonFunctionCall('random', 'randint', CreateObject('PythonTuple', 5, 20)) is like calling random.randint(5, 20) in python)
- Get items and attributes with obj.getitem and obj.getattr (e.g obj.getitem('key') will retrieve the dictionary item with key 'key')
