from __future__ import print_function

import sys
import os

import argparse

from inspect import getmembers, isfunction, ismodule
from importlib import import_module

def parse_args(argv=None):
    parser = argparse.ArgumentParser(description='Create foxpro template from python module')
    parser.add_argument('--path', metavar='additional_path', type=str)
    parser.add_argument('mod', metavar='module_name', type=str)
    return parser.parse_args(argv)

def print_code(code, depth=0):
    ret = ''
    for part in code:
        if isinstance(part, str):
            ret += '   '*depth + part + '\n'
        else:
            ret += print_code(part, depth+1)

    return ret

def create_code(mod, original_mod=None):
    if not original_mod:
        original_mod = mod
    code = []
    for mem in getmembers(mod):
        if mem[0].startswith('_'):
            continue

        if ismodule(mem[1]) and mem[1] != mod:
            if mem[1].__name__.startswith(mod.__name__) and \
                    os.path.basename(mem[1].__file__).startswith('__init__.py'):
                code += create_code(mem[1], original_mod)
            continue

        if not isfunction(mem[1]):
            continue

        if original_mod != mod:
            mod_name = mod.__name__.replace('.', '_').replace(original_mod.__name__ + '_', '', 1)
            funcname = mod_name + '_' + mem[0]
        else:
            funcname = mem[0]
        args = list(mem[1].func_code.co_varnames[:mem[1].func_code.co_argcount])
        if len(args) > 9:
            print('Cannot convert function \'{}\' with more than 9 arguments'.format(funcname), file=sys.stderr)
            continue

        code.append('FUNCTION {}({})'.format(funcname, ', '.join(args)))

        args.insert(0, '\'PythonTuple\'')

        body = 'RETURN PythonFunctionCall(\'{}\', \'{}\', CREATEOBJECT({}))'
        code.append([body.format(mod.__name__, mem[0], ', '.join(args))])
        code.append('ENDFUNC')
        code.append('')
    return code

def main(argv=None):
    args = parse_args(argv)
    if args.path:
        sys.path.append(args.path)
    else:
        sys.path.append('.')
    mod = import_module(args.mod)

    code = create_code(mod)

    print(print_code(code)[:-1], end='')

if __name__ == '__main__':
    main()
