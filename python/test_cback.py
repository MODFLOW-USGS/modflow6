#!/usr/bin/env python

"""Simple callback test
"""

import os
import sys

import numpy as np

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
        mf6.access_memory.get_int_2d('LRCH', 'SLN_1', ncol=3, nrow=500)
        lrch = mf6.shared_data.int_2d
        print('LRCH', lrch.shape)
        print(lrch[0,:20])
        lrch[0, 4:10] = 22
        mf6.access_memory.set_int_2d('LRCH', 'SLN_1', lrch)
        mf6.access_memory.get_int_2d('LRCH', 'SLN_1', ncol=3, nrow=500)
        print(mf6.shared_data.int_2d[0,:20])

        mf6.access_memory.get_float_2d('AUXVAR', 'GWF_1 WEL', ncol=3, nrow=5)
        auxvar = mf6.shared_data.float_2d
        print('AUXVAR', auxvar)
        auxvar[1, 1] = 17.8
        auxvar[2, 2] = 7.9
        mf6.access_memory.set_float_2d('AUXVAR', 'GWF_1 WEL', auxvar)

        if self.counter > 3:
            mf6.access_memory.get_int_1d('NSTP', 'TDIS', 4)
            print('NSTP', mf6.shared_data.int_1d)
            #input('...')
            mf6.access_memory.get_float_1d('PERLEN', 'TDIS', 4)
            print('PERLEN', mf6.shared_data.float_1d)
            #input('...')
            #mf6.access_memory.set_int('NPER', 'TDIS', 5)
            mf6.access_memory.set_float('DELT', 'TDIS', 0.05)
        if self.counter > 5:
            mf6.access_memory.set_int_1d('NSTP', 'TDIS', [2, 110, 111, 112])
            mf6.access_memory.set_float_1d('PERLEN', 'TDIS',
                np.array([1.2, 9.8, 11.7, 10.8]))

        #print('maxobstypes', mf6.constantsmodule.maxobstypes)
        #print('basegeometrymodule', mf6.basegeometrymodule.area_sat)




if __name__ == '__main__':
    mf6.mf6_sub(Func(10))
