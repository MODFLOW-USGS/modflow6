#!/usr/bin/env python

"""Simple callback test
"""

import os
import sys

import numpy as np

from fortran_io import get_value, set_value
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

    def __call__(self):
        self.counter += 1
        print(f'>>> Python: Called {self.counter} time')
        if self.counter > 3:
            print('int', get_value('NPER', 'TDIS'))
            print('float', get_value('DELT', 'TDIS'))
            lrch = get_value('LRCH', 'SLN_1')
            print('LRCH', lrch.shape)
            print(lrch[0, :20])
            lrch[0, 4:10] = 22
            set_value('LRCH', 'SLN_1', lrch)
            print(get_value('LRCH', 'SLN_1')[0, :20])
            auxvar = get_value('AUXVAR', 'GWF_1 WEL')
            print('AUXVAR', auxvar)
            auxvar[1, 1] = 17.8
            auxvar[2, 2] = 7.9
            set_value('AUXVAR', 'GWF_1 WEL', auxvar)
            print('NSTP', get_value('NSTP', 'TDIS'))
            print('PERLEN', get_value('PERLEN', 'TDIS'))
            # Uncommenting the next line leads to the error:
            #
            # Unexpected end of file reached.
            # ERROR OCCURRED WHILE READING FILE:
            # AdvGW_tidal.oc
            # --> IT WORKS!
            # set_value('NPER', 'TDIS', 5)
            set_value('DELT', 'TDIS', 0.05)
        if self.counter > 5:
            set_value('NSTP', 'TDIS', [2, 110, 111, 112])
            data = np.array([1.2, 9.8, 11.7, 10.8])
            set_value('PERLEN', 'TDIS', data)


if __name__ == '__main__':
    mf6.mf6_sub(Func(10))
