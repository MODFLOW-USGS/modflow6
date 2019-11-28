import sys
import numpy as np
from ctypes import *


nargs = len(sys.argv) - 1
if nargs < 1:
    sys.exit()

dllpath = sys.argv[1]
print("running from mf6 dll: ", dllpath)
mf6 = cdll.LoadLibrary(dllpath)

# initialize the model
mf6.initialize()

# get times
ct = c_double(0.0)
mf6.get_current_time(byref(ct))
et = c_double(0.0)
mf6.get_end_time(byref(et))

# get variable size(s)
elsize = c_int(0)
nbytes = c_int(0)
k11name = c_char_p(b"MV NPF/K11")
k33name = c_char_p(b"MV NPF/K33")
mf6.get_var_itemsize(k11name, byref(elsize))
mf6.get_var_nbytes(k11name, byref(nbytes))
nsize = int(nbytes.value/elsize.value)

# set the function prototype and declare the receiving pointers-to-array
arraytype = np.ctypeslib.ndpointer(dtype="double", ndim=1, shape=(nsize,), flags='F')
mf6.get_value_ptr_double.argtypes = [c_char_p, POINTER(arraytype)]
mf6.get_value_ptr_double.restype = c_int
mf6.get_value_double.argtypes = [c_char_p, arraytype, POINTER(c_int)]
mf6.get_value_double.restype = c_int

k11 = arraytype()
k33 = arraytype()

localArray = np.zeros(nsize, dtype="double", order='F')

# model time loop
while ct.value < et.value:
    print("time: ", ct.value)
    # calculate
    mf6.update()

    # get data through pointer
    mf6.get_value_ptr_double(k11name, byref(k11))  
    k11array = k11.contents
    print(k11array)
    mf6.get_value_ptr_double(k33name, byref(k33))
    k33array = k33.contents
    # print(k33array)

    # get data copied into our own array
    mf6.get_value_double(k11name, localArray, byref(c_int(nsize)))
    # print(localArray)

    # update time
    mf6.get_current_time(byref(ct))

# cleanup
mf6.finalize()