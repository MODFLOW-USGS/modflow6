#!/usr/bin/env python

"""Simple callback test
"""

import os
import sys

import mf6


sys.path.insert(0, os.getcwd())



class Func:
    # pylint: disable=too-few-public-methods
    """Class whose instances act like a function, i.e. are callables

    https://docs.python.org/3/reference/datamodel.html?highlight=__call__#object.__call__

    Called when the instance is “called” as a function; if this method is
    defined, `x(arg1, arg2, ...)` is a shorthand for
    `x.__call__(arg1, arg2, ...).`


    """

    def __init__(self, val):
        self.val = val
        self.counter = 0
        mf6.shared_data.init_data()

    def __call__(self):
        self.counter += 1
        print(f'>>> Python: Called {self.counter} time')
        print(f'>>> Python: Value {self.val}')
        print('int before', mf6.shared_data.anint)
        #input('..')
        mf6.access_memory.get_int('NPER', 'TDIS')
        print('int after', mf6.shared_data.anint)
        print('float before', mf6.shared_data.afloat)
        #input('..')
        mf6.access_memory.get_float('DELT', 'TDIS')
        print('float after', mf6.shared_data.afloat)
        if self.counter > 3:
             #mf6.access_memory.set_int('NPER', 'TDIS', 5)
             mf6.access_memory.set_float('DELT', 'TDIS', 0.05)
        #print('maxobstypes', mf6.constantsmodule.maxobstypes)
        #print('basegeometrymodule', mf6.basegeometrymodule.area_sat)




if __name__ == '__main__':
    mf6.mf6_sub(Func(10))
