import os
import sys
import numpy as np
import targets

try:
    import pymake
except:
    msg = 'Error. Pymake package is not available.\n'
    msg += 'Try installing using the following command:\n'
    msg += ' pip install https://github.com/modflowpy/pymake/zipball/master'
    raise Exception(msg)

try:
    import flopy
except:
    msg = 'Error. FloPy package is not available.\n'
    msg += 'Try installing using the following command:\n'
    msg += ' pip install flopy'
    raise Exception(msg)

import flopy.utils.binaryfile as bf
from framework import testing_framework
from simulation import Simulation

mf6_exe = os.path.abspath(targets.target_dict['mf6'])
mfnwt_exe = os.path.abspath(targets.target_dict['mfnwt'])

ex = ['uzf_8ly_uzet_chk']
exdirs = []
iuz_cell_dict = {}
cell_iuz_dict = {}

for s in ex:
    exdirs.append(os.path.join('temp', s))

nlay, nrow, ncol = 8, 1, 10
nper = 6
perlen = [30.] * 6
nstp = [10] + [1] * 5
tsmult = [1.] * len(perlen)

delr = 1.
delc = 1.
strt = -25

nouter, ninner = 100, 300
hclose, rclose, relax = 1e-9, 1e-3, 0.97

top = 0.
botm = [-1., -2., -3., -4., -5., -10., -20., -30.]

icelltype = 1
k = 100.0
k33 = 10
ss = 1e-5
sy = 0.3

ghbelv1 = -22.0
ghbelv2 = -15.0
ghbcond = 1000.
ghbspd = {0: [[(7, 0, 0), ghbelv1, ghbcond], 
              [(7, 0, ncol - 1), ghbelv1, ghbcond]],
          3: [[(7, 0, 0), ghbelv2, ghbcond], 
              [(7, 0, ncol - 1), ghbelv2, ghbcond]]}

# iuzno  cellid landflg ivertcn surfdp vks thtr thts thti eps [bndnm]
surfdep1 = 0.2
surfdep2 = 0.000
vks = 0.5
eps = 4.
thti = 0.06
thtr = 0.05
thts = 0.35
iuzfbnd = [[0, 1, 1, 1, 1, 1, 1, 1, 1, 0]]
uzf_pkdat = [
    [ 0, (0, 0, 1), 1,  8, surfdep1, vks, thtr, thts, thti, eps, "uzf01"],
    [ 1, (0, 0, 2), 1,  9, surfdep1, vks, thtr, thts, thti, eps, "uzf02"],
    [ 2, (0, 0, 3), 1, 10, surfdep1, vks, thtr, thts, thti, eps, "uzf03"],
    [ 3, (0, 0, 4), 1, 11, surfdep1, vks, thtr, thts, thti, eps, "uzf04"],
    [ 4, (0, 0, 5), 1, 12, surfdep1, vks, thtr, thts, thti, eps, "uzf05"],
    [ 5, (0, 0, 6), 1, 13, surfdep1, vks, thtr, thts, thti, eps, "uzf06"],
    [ 6, (0, 0, 7), 1, 14, surfdep1, vks, thtr, thts, thti, eps, "uzf07"],
    [ 7, (0, 0, 8), 1, 15, surfdep1, vks, thtr, thts, thti, eps, "uzf08"],
    [ 8, (1, 0, 1), 0, 16, surfdep2, vks, thtr, thts, thti, eps, "uzf09"],
    [ 9, (1, 0, 2), 0, 17, surfdep2, vks, thtr, thts, thti, eps, "uzf10"],
    [10, (1, 0, 3), 0, 18, surfdep2, vks, thtr, thts, thti, eps, "uzf11"],
    [11, (1, 0, 4), 0, 19, surfdep2, vks, thtr, thts, thti, eps, "uzf12"],
    [12, (1, 0, 5), 0, 20, surfdep2, vks, thtr, thts, thti, eps, "uzf13"],
    [13, (1, 0, 6), 0, 21, surfdep2, vks, thtr, thts, thti, eps, "uzf14"],
    [14, (1, 0, 7), 0, 22, surfdep2, vks, thtr, thts, thti, eps, "uzf15"],
    [15, (1, 0, 8), 0, 23, surfdep2, vks, thtr, thts, thti, eps, "uzf16"],
    [16, (2, 0, 1), 0, 24, surfdep2, vks, thtr, thts, thti, eps, "uzf17"],
    [17, (2, 0, 2), 0, 25, surfdep2, vks, thtr, thts, thti, eps, "uzf18"],
    [18, (2, 0, 3), 0, 26, surfdep2, vks, thtr, thts, thti, eps, "uzf19"],
    [19, (2, 0, 4), 0, 27, surfdep2, vks, thtr, thts, thti, eps, "uzf20"],
    [20, (2, 0, 5), 0, 28, surfdep2, vks, thtr, thts, thti, eps, "uzf21"],
    [21, (2, 0, 6), 0, 29, surfdep2, vks, thtr, thts, thti, eps, "uzf22"],
    [22, (2, 0, 7), 0, 30, surfdep2, vks, thtr, thts, thti, eps, "uzf23"],
    [23, (2, 0, 8), 0, 31, surfdep2, vks, thtr, thts, thti, eps, "uzf24"],
    [24, (3, 0, 1), 0, 32, surfdep2, vks, thtr, thts, thti, eps, "uzf25"],
    [25, (3, 0, 2), 0, 33, surfdep2, vks, thtr, thts, thti, eps, "uzf26"],
    [26, (3, 0, 3), 0, 34, surfdep2, vks, thtr, thts, thti, eps, "uzf27"],
    [27, (3, 0, 4), 0, 35, surfdep2, vks, thtr, thts, thti, eps, "uzf28"],
    [28, (3, 0, 5), 0, 36, surfdep2, vks, thtr, thts, thti, eps, "uzf29"],
    [29, (3, 0, 6), 0, 37, surfdep2, vks, thtr, thts, thti, eps, "uzf30"],
    [30, (3, 0, 7), 0, 38, surfdep2, vks, thtr, thts, thti, eps, "uzf31"],
    [31, (3, 0, 8), 0, 39, surfdep2, vks, thtr, thts, thti, eps, "uzf32"],
    [32, (4, 0, 1), 0, 40, surfdep2, vks, thtr, thts, thti, eps, "uzf33"],
    [33, (4, 0, 2), 0, 41, surfdep2, vks, thtr, thts, thti, eps, "uzf34"],
    [34, (4, 0, 3), 0, 42, surfdep2, vks, thtr, thts, thti, eps, "uzf35"],
    [35, (4, 0, 4), 0, 43, surfdep2, vks, thtr, thts, thti, eps, "uzf36"],
    [36, (4, 0, 5), 0, 44, surfdep2, vks, thtr, thts, thti, eps, "uzf37"],
    [37, (4, 0, 6), 0, 45, surfdep2, vks, thtr, thts, thti, eps, "uzf38"],
    [38, (4, 0, 7), 0, 46, surfdep2, vks, thtr, thts, thti, eps, "uzf39"],
    [39, (4, 0, 8), 0, 47, surfdep2, vks, thtr, thts, thti, eps, "uzf40"],
    [40, (5, 0, 1), 0, 48, surfdep2, vks, thtr, thts, thti, eps, "uzf41"],
    [41, (5, 0, 2), 0, 49, surfdep2, vks, thtr, thts, thti, eps, "uzf42"],
    [42, (5, 0, 3), 0, 50, surfdep2, vks, thtr, thts, thti, eps, "uzf43"],
    [43, (5, 0, 4), 0, 51, surfdep2, vks, thtr, thts, thti, eps, "uzf44"],
    [44, (5, 0, 5), 0, 52, surfdep2, vks, thtr, thts, thti, eps, "uzf45"],
    [45, (5, 0, 6), 0, 53, surfdep2, vks, thtr, thts, thti, eps, "uzf46"],
    [46, (5, 0, 7), 0, 54, surfdep2, vks, thtr, thts, thti, eps, "uzf47"],
    [47, (5, 0, 8), 0, 55, surfdep2, vks, thtr, thts, thti, eps, "uzf48"],
    [48, (6, 0, 1), 0, 56, surfdep2, vks, thtr, thts, thti, eps, "uzf49"],
    [49, (6, 0, 2), 0, 57, surfdep2, vks, thtr, thts, thti, eps, "uzf50"],
    [50, (6, 0, 3), 0, 58, surfdep2, vks, thtr, thts, thti, eps, "uzf51"],
    [51, (6, 0, 4), 0, 59, surfdep2, vks, thtr, thts, thti, eps, "uzf52"],
    [52, (6, 0, 5), 0, 60, surfdep2, vks, thtr, thts, thti, eps, "uzf53"],
    [53, (6, 0, 6), 0, 61, surfdep2, vks, thtr, thts, thti, eps, "uzf54"],
    [54, (6, 0, 7), 0, 62, surfdep2, vks, thtr, thts, thti, eps, "uzf55"],
    [55, (6, 0, 8), 0, 63, surfdep2, vks, thtr, thts, thti, eps, "uzf56"],
    [56, (7, 0, 1), 0, -1, surfdep2, vks, thtr, thts, thti, eps, "uzf57"],
    [57, (7, 0, 2), 0, -1, surfdep2, vks, thtr, thts, thti, eps, "uzf58"],
    [58, (7, 0, 3), 0, -1, surfdep2, vks, thtr, thts, thti, eps, "uzf59"],
    [59, (7, 0, 4), 0, -1, surfdep2, vks, thtr, thts, thti, eps, "uzf60"],
    [60, (7, 0, 5), 0, -1, surfdep2, vks, thtr, thts, thti, eps, "uzf61"],
    [61, (7, 0, 6), 0, -1, surfdep2, vks, thtr, thts, thti, eps, "uzf62"],
    [62, (7, 0, 7), 0, -1, surfdep2, vks, thtr, thts, thti, eps, "uzf63"],
    [63, (7, 0, 8), 0, -1, surfdep2, vks, thtr, thts, thti, eps, "uzf64"]
]

for itm in uzf_pkdat:
    iuz_cell_dict.update({itm[0]: (itm[1][0], itm[1][1], itm[1][2])})
    cell_iuz_dict.update({(itm[1][0], itm[1][1], itm[1][2]): itm[0]})


ntrail2 = 25
nsets2 = 80
extdp = 5.
extwc = 0.08
pet0 = 0.0
pet1 = 0.001
pet2 = 0.010
finf0 = 0.5
finf1 = 0.000
finf2 = 0.05
finf3 = 0.01
zero = 0.
uzf1_finf = {0: finf0, 1: finf1, 2: finf2, 3:finf3, 4:finf1, 5:finf1}
uzf1_pet = {0: pet0, 1: pet1, 2: pet1, 3: pet1, 4: pet2, 5: pet2}
uzf_spd = {
    0:[[ 0, finf0, pet0, extdp, extwc, zero, zero, zero],
       [ 1, finf0, pet0, extdp, extwc, zero, zero, zero],
       [ 2, finf0, pet0, extdp, extwc, zero, zero, zero],
       [ 3, finf0, pet0, extdp, extwc, zero, zero, zero],
       [ 4, finf0, pet0, extdp, extwc, zero, zero, zero],
       [ 5, finf0, pet0, extdp, extwc, zero, zero, zero],
       [ 6, finf0, pet0, extdp, extwc, zero, zero, zero],
       [ 7, finf0, pet0, extdp, extwc, zero, zero, zero],
       [ 8, zero,  pet0, extdp, extwc, zero, zero, zero],
       [ 9, zero,  pet0, extdp, extwc, zero, zero, zero],
       [10, zero,  pet0, extdp, extwc, zero, zero, zero],
       [11, zero,  pet0, extdp, extwc, zero, zero, zero],
       [12, zero,  pet0, extdp, extwc, zero, zero, zero],
       [13, zero,  pet0, extdp, extwc, zero, zero, zero],
       [14, zero,  pet0, extdp, extwc, zero, zero, zero],
       [15, zero,  pet0, extdp, extwc, zero, zero, zero],
       [16, zero,  pet0, extdp, extwc, zero, zero, zero],
       [17, zero,  pet0, extdp, extwc, zero, zero, zero],
       [18, zero,  pet0, extdp, extwc, zero, zero, zero],
       [19, zero,  pet0, extdp, extwc, zero, zero, zero],
       [20, zero,  pet0, extdp, extwc, zero, zero, zero],
       [21, zero,  pet0, extdp, extwc, zero, zero, zero],
       [22, zero,  pet0, extdp, extwc, zero, zero, zero],
       [23, zero,  pet0, extdp, extwc, zero, zero, zero],
       [24, zero,  pet0, extdp, extwc, zero, zero, zero],
       [25, zero,  pet0, extdp, extwc, zero, zero, zero],
       [26, zero,  pet0, extdp, extwc, zero, zero, zero],
       [27, zero,  pet0, extdp, extwc, zero, zero, zero],
       [28, zero,  pet0, extdp, extwc, zero, zero, zero],
       [29, zero,  pet0, extdp, extwc, zero, zero, zero],
       [30, zero,  pet0, extdp, extwc, zero, zero, zero],
       [31, zero,  pet0, extdp, extwc, zero, zero, zero],
       [32, zero,  pet0, extdp, extwc, zero, zero, zero],
       [33, zero,  pet0, extdp, extwc, zero, zero, zero],
       [34, zero,  pet0, extdp, extwc, zero, zero, zero],
       [35, zero,  pet0, extdp, extwc, zero, zero, zero],
       [36, zero,  pet0, extdp, extwc, zero, zero, zero],
       [37, zero,  pet0, extdp, extwc, zero, zero, zero],
       [38, zero,  pet0, extdp, extwc, zero, zero, zero],
       [39, zero,  pet0, extdp, extwc, zero, zero, zero],
       [40, zero,  zero, extdp, extwc, zero, zero, zero],
       [41, zero,  zero, extdp, extwc, zero, zero, zero],
       [42, zero,  zero, extdp, extwc, zero, zero, zero],
       [43, zero,  zero, extdp, extwc, zero, zero, zero],
       [44, zero,  zero, extdp, extwc, zero, zero, zero],
       [45, zero,  zero, extdp, extwc, zero, zero, zero],
       [46, zero,  zero, extdp, extwc, zero, zero, zero],
       [47, zero,  zero, extdp, extwc, zero, zero, zero],
       [48, zero,  zero, extdp, extwc, zero, zero, zero],
       [49, zero,  zero, extdp, extwc, zero, zero, zero],
       [50, zero,  zero, extdp, extwc, zero, zero, zero],
       [51, zero,  zero, extdp, extwc, zero, zero, zero],
       [52, zero,  zero, extdp, extwc, zero, zero, zero],
       [53, zero,  zero, extdp, extwc, zero, zero, zero],
       [54, zero,  zero, extdp, extwc, zero, zero, zero],
       [55, zero,  zero, extdp, extwc, zero, zero, zero],
       [56, zero,  zero, extdp, extwc, zero, zero, zero],
       [57, zero,  zero, extdp, extwc, zero, zero, zero],
       [58, zero,  zero, extdp, extwc, zero, zero, zero],
       [59, zero,  zero, extdp, extwc, zero, zero, zero],
       [60, zero,  zero, extdp, extwc, zero, zero, zero],
       [61, zero,  zero, extdp, extwc, zero, zero, zero],
       [62, zero,  zero, extdp, extwc, zero, zero, zero],
       [63, zero,  zero, extdp, extwc, zero, zero, zero]],
    
    1:[[ 0, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 1, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 2, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 3, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 4, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 5, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 6, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 7, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 8, zero,  pet2, extdp, extwc, zero, zero, zero],
       [ 9, zero,  pet2, extdp, extwc, zero, zero, zero],
       [10, zero,  pet2, extdp, extwc, zero, zero, zero],
       [11, zero,  pet2, extdp, extwc, zero, zero, zero],
       [12, zero,  pet2, extdp, extwc, zero, zero, zero],
       [13, zero,  pet2, extdp, extwc, zero, zero, zero],
       [14, zero,  pet2, extdp, extwc, zero, zero, zero],
       [15, zero,  pet2, extdp, extwc, zero, zero, zero],
       [16, zero,  pet2, extdp, extwc, zero, zero, zero],
       [17, zero,  pet2, extdp, extwc, zero, zero, zero],
       [18, zero,  pet2, extdp, extwc, zero, zero, zero],
       [19, zero,  pet2, extdp, extwc, zero, zero, zero],
       [20, zero,  pet2, extdp, extwc, zero, zero, zero],
       [21, zero,  pet2, extdp, extwc, zero, zero, zero],
       [22, zero,  pet2, extdp, extwc, zero, zero, zero],
       [23, zero,  pet2, extdp, extwc, zero, zero, zero],
       [24, zero,  pet2, extdp, extwc, zero, zero, zero],
       [25, zero,  pet2, extdp, extwc, zero, zero, zero],
       [26, zero,  pet2, extdp, extwc, zero, zero, zero],
       [27, zero,  pet2, extdp, extwc, zero, zero, zero],
       [28, zero,  pet2, extdp, extwc, zero, zero, zero],
       [29, zero,  pet2, extdp, extwc, zero, zero, zero],
       [30, zero,  pet2, extdp, extwc, zero, zero, zero],
       [31, zero,  pet2, extdp, extwc, zero, zero, zero],
       [32, zero,  pet2, extdp, extwc, zero, zero, zero],
       [33, zero,  pet2, extdp, extwc, zero, zero, zero],
       [34, zero,  pet2, extdp, extwc, zero, zero, zero],
       [35, zero,  pet2, extdp, extwc, zero, zero, zero],
       [36, zero,  pet2, extdp, extwc, zero, zero, zero],
       [37, zero,  pet2, extdp, extwc, zero, zero, zero],
       [38, zero,  pet2, extdp, extwc, zero, zero, zero],
       [39, zero,  pet2, extdp, extwc, zero, zero, zero],
       [40, zero,  zero, extdp, extwc, zero, zero, zero],
       [41, zero,  zero, extdp, extwc, zero, zero, zero],
       [42, zero,  zero, extdp, extwc, zero, zero, zero],
       [43, zero,  zero, extdp, extwc, zero, zero, zero],
       [44, zero,  zero, extdp, extwc, zero, zero, zero],
       [45, zero,  zero, extdp, extwc, zero, zero, zero],
       [46, zero,  zero, extdp, extwc, zero, zero, zero],
       [47, zero,  zero, extdp, extwc, zero, zero, zero],
       [48, zero,  zero, extdp, extwc, zero, zero, zero],
       [49, zero,  zero, extdp, extwc, zero, zero, zero],
       [50, zero,  zero, extdp, extwc, zero, zero, zero],
       [51, zero,  zero, extdp, extwc, zero, zero, zero],
       [52, zero,  zero, extdp, extwc, zero, zero, zero],
       [53, zero,  zero, extdp, extwc, zero, zero, zero],
       [54, zero,  zero, extdp, extwc, zero, zero, zero],
       [55, zero,  zero, extdp, extwc, zero, zero, zero],
       [56, zero,  zero, extdp, extwc, zero, zero, zero],
       [57, zero,  zero, extdp, extwc, zero, zero, zero],
       [58, zero,  zero, extdp, extwc, zero, zero, zero],
       [59, zero,  zero, extdp, extwc, zero, zero, zero],
       [60, zero,  zero, extdp, extwc, zero, zero, zero],
       [61, zero,  zero, extdp, extwc, zero, zero, zero],
       [62, zero,  zero, extdp, extwc, zero, zero, zero],
       [63, zero,  zero, extdp, extwc, zero, zero, zero]],

    2:[[ 0, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 1, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 2, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 3, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 4, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 5, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 6, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 7, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 8, zero,  pet2, extdp, extwc, zero, zero, zero],
       [ 9, zero,  pet2, extdp, extwc, zero, zero, zero],
       [10, zero,  pet2, extdp, extwc, zero, zero, zero],
       [11, zero,  pet2, extdp, extwc, zero, zero, zero],
       [12, zero,  pet2, extdp, extwc, zero, zero, zero],
       [13, zero,  pet2, extdp, extwc, zero, zero, zero],
       [14, zero,  pet2, extdp, extwc, zero, zero, zero],
       [15, zero,  pet2, extdp, extwc, zero, zero, zero],
       [16, zero,  pet2, extdp, extwc, zero, zero, zero],
       [17, zero,  pet2, extdp, extwc, zero, zero, zero],
       [18, zero,  pet2, extdp, extwc, zero, zero, zero],
       [19, zero,  pet2, extdp, extwc, zero, zero, zero],
       [20, zero,  pet2, extdp, extwc, zero, zero, zero],
       [21, zero,  pet2, extdp, extwc, zero, zero, zero],
       [22, zero,  pet2, extdp, extwc, zero, zero, zero],
       [23, zero,  pet2, extdp, extwc, zero, zero, zero],
       [24, zero,  pet2, extdp, extwc, zero, zero, zero],
       [25, zero,  pet2, extdp, extwc, zero, zero, zero],
       [26, zero,  pet2, extdp, extwc, zero, zero, zero],
       [27, zero,  pet2, extdp, extwc, zero, zero, zero],
       [28, zero,  pet2, extdp, extwc, zero, zero, zero],
       [29, zero,  pet2, extdp, extwc, zero, zero, zero],
       [30, zero,  pet2, extdp, extwc, zero, zero, zero],
       [31, zero,  pet2, extdp, extwc, zero, zero, zero],
       [32, zero,  pet2, extdp, extwc, zero, zero, zero],
       [33, zero,  pet2, extdp, extwc, zero, zero, zero],
       [34, zero,  pet2, extdp, extwc, zero, zero, zero],
       [35, zero,  pet2, extdp, extwc, zero, zero, zero],
       [36, zero,  pet2, extdp, extwc, zero, zero, zero],
       [37, zero,  pet2, extdp, extwc, zero, zero, zero],
       [38, zero,  pet2, extdp, extwc, zero, zero, zero],
       [39, zero,  pet2, extdp, extwc, zero, zero, zero],
       [40, zero,  zero, extdp, extwc, zero, zero, zero],
       [41, zero,  zero, extdp, extwc, zero, zero, zero],
       [42, zero,  zero, extdp, extwc, zero, zero, zero],
       [43, zero,  zero, extdp, extwc, zero, zero, zero],
       [44, zero,  zero, extdp, extwc, zero, zero, zero],
       [45, zero,  zero, extdp, extwc, zero, zero, zero],
       [46, zero,  zero, extdp, extwc, zero, zero, zero],
       [47, zero,  zero, extdp, extwc, zero, zero, zero],
       [48, zero,  zero, extdp, extwc, zero, zero, zero],
       [49, zero,  zero, extdp, extwc, zero, zero, zero],
       [50, zero,  zero, extdp, extwc, zero, zero, zero],
       [51, zero,  zero, extdp, extwc, zero, zero, zero],
       [52, zero,  zero, extdp, extwc, zero, zero, zero],
       [53, zero,  zero, extdp, extwc, zero, zero, zero],
       [54, zero,  zero, extdp, extwc, zero, zero, zero],
       [55, zero,  zero, extdp, extwc, zero, zero, zero],
       [56, zero,  zero, extdp, extwc, zero, zero, zero],
       [57, zero,  zero, extdp, extwc, zero, zero, zero],
       [58, zero,  zero, extdp, extwc, zero, zero, zero],
       [59, zero,  zero, extdp, extwc, zero, zero, zero],
       [60, zero,  zero, extdp, extwc, zero, zero, zero],
       [61, zero,  zero, extdp, extwc, zero, zero, zero],
       [62, zero,  zero, extdp, extwc, zero, zero, zero],
       [63, zero,  zero, extdp, extwc, zero, zero, zero]],
    
    3:[[ 0, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 1, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 2, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 3, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 4, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 5, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 6, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 7, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 8, zero,  pet2, extdp, extwc, zero, zero, zero],
       [ 9, zero,  pet2, extdp, extwc, zero, zero, zero],
       [10, zero,  pet2, extdp, extwc, zero, zero, zero],
       [11, zero,  pet2, extdp, extwc, zero, zero, zero],
       [12, zero,  pet2, extdp, extwc, zero, zero, zero],
       [13, zero,  pet2, extdp, extwc, zero, zero, zero],
       [14, zero,  pet2, extdp, extwc, zero, zero, zero],
       [15, zero,  pet2, extdp, extwc, zero, zero, zero],
       [16, zero,  pet2, extdp, extwc, zero, zero, zero],
       [17, zero,  pet2, extdp, extwc, zero, zero, zero],
       [18, zero,  pet2, extdp, extwc, zero, zero, zero],
       [19, zero,  pet2, extdp, extwc, zero, zero, zero],
       [20, zero,  pet2, extdp, extwc, zero, zero, zero],
       [21, zero,  pet2, extdp, extwc, zero, zero, zero],
       [22, zero,  pet2, extdp, extwc, zero, zero, zero],
       [23, zero,  pet2, extdp, extwc, zero, zero, zero],
       [24, zero,  pet2, extdp, extwc, zero, zero, zero],
       [25, zero,  pet2, extdp, extwc, zero, zero, zero],
       [26, zero,  pet2, extdp, extwc, zero, zero, zero],
       [27, zero,  pet2, extdp, extwc, zero, zero, zero],
       [28, zero,  pet2, extdp, extwc, zero, zero, zero],
       [29, zero,  pet2, extdp, extwc, zero, zero, zero],
       [30, zero,  pet2, extdp, extwc, zero, zero, zero],
       [31, zero,  pet2, extdp, extwc, zero, zero, zero],
       [32, zero,  pet2, extdp, extwc, zero, zero, zero],
       [33, zero,  pet2, extdp, extwc, zero, zero, zero],
       [34, zero,  pet2, extdp, extwc, zero, zero, zero],
       [35, zero,  pet2, extdp, extwc, zero, zero, zero],
       [36, zero,  pet2, extdp, extwc, zero, zero, zero],
       [37, zero,  pet2, extdp, extwc, zero, zero, zero],
       [38, zero,  pet2, extdp, extwc, zero, zero, zero],
       [39, zero,  pet2, extdp, extwc, zero, zero, zero],
       [40, zero,  zero, extdp, extwc, zero, zero, zero],
       [41, zero,  zero, extdp, extwc, zero, zero, zero],
       [42, zero,  zero, extdp, extwc, zero, zero, zero],
       [43, zero,  zero, extdp, extwc, zero, zero, zero],
       [44, zero,  zero, extdp, extwc, zero, zero, zero],
       [45, zero,  zero, extdp, extwc, zero, zero, zero],
       [46, zero,  zero, extdp, extwc, zero, zero, zero],
       [47, zero,  zero, extdp, extwc, zero, zero, zero],
       [48, zero,  zero, extdp, extwc, zero, zero, zero],
       [49, zero,  zero, extdp, extwc, zero, zero, zero],
       [50, zero,  zero, extdp, extwc, zero, zero, zero],
       [51, zero,  zero, extdp, extwc, zero, zero, zero],
       [52, zero,  zero, extdp, extwc, zero, zero, zero],
       [53, zero,  zero, extdp, extwc, zero, zero, zero],
       [54, zero,  zero, extdp, extwc, zero, zero, zero],
       [55, zero,  zero, extdp, extwc, zero, zero, zero],
       [56, zero,  zero, extdp, extwc, zero, zero, zero],
       [57, zero,  zero, extdp, extwc, zero, zero, zero],
       [58, zero,  zero, extdp, extwc, zero, zero, zero],
       [59, zero,  zero, extdp, extwc, zero, zero, zero],
       [60, zero,  zero, extdp, extwc, zero, zero, zero],
       [61, zero,  zero, extdp, extwc, zero, zero, zero],
       [62, zero,  zero, extdp, extwc, zero, zero, zero],
       [63, zero,  zero, extdp, extwc, zero, zero, zero]],
           
    4:[[ 0, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 1, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 2, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 3, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 4, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 5, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 6, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 7, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 8, zero,  pet2, extdp, extwc, zero, zero, zero],
       [ 9, zero,  pet2, extdp, extwc, zero, zero, zero],
       [10, zero,  pet2, extdp, extwc, zero, zero, zero],
       [11, zero,  pet2, extdp, extwc, zero, zero, zero],
       [12, zero,  pet2, extdp, extwc, zero, zero, zero],
       [13, zero,  pet2, extdp, extwc, zero, zero, zero],
       [14, zero,  pet2, extdp, extwc, zero, zero, zero],
       [15, zero,  pet2, extdp, extwc, zero, zero, zero],
       [16, zero,  pet2, extdp, extwc, zero, zero, zero],
       [17, zero,  pet2, extdp, extwc, zero, zero, zero],
       [18, zero,  pet2, extdp, extwc, zero, zero, zero],
       [19, zero,  pet2, extdp, extwc, zero, zero, zero],
       [20, zero,  pet2, extdp, extwc, zero, zero, zero],
       [21, zero,  pet2, extdp, extwc, zero, zero, zero],
       [22, zero,  pet2, extdp, extwc, zero, zero, zero],
       [23, zero,  pet2, extdp, extwc, zero, zero, zero],
       [24, zero,  pet2, extdp, extwc, zero, zero, zero],
       [25, zero,  pet2, extdp, extwc, zero, zero, zero],
       [26, zero,  pet2, extdp, extwc, zero, zero, zero],
       [27, zero,  pet2, extdp, extwc, zero, zero, zero],
       [28, zero,  pet2, extdp, extwc, zero, zero, zero],
       [29, zero,  pet2, extdp, extwc, zero, zero, zero],
       [30, zero,  pet2, extdp, extwc, zero, zero, zero],
       [31, zero,  pet2, extdp, extwc, zero, zero, zero],
       [32, zero,  pet2, extdp, extwc, zero, zero, zero],
       [33, zero,  pet2, extdp, extwc, zero, zero, zero],
       [34, zero,  pet2, extdp, extwc, zero, zero, zero],
       [35, zero,  pet2, extdp, extwc, zero, zero, zero],
       [36, zero,  pet2, extdp, extwc, zero, zero, zero],
       [37, zero,  pet2, extdp, extwc, zero, zero, zero],
       [38, zero,  pet2, extdp, extwc, zero, zero, zero],
       [39, zero,  pet2, extdp, extwc, zero, zero, zero],
       [40, zero,  zero, extdp, extwc, zero, zero, zero],
       [41, zero,  zero, extdp, extwc, zero, zero, zero],
       [42, zero,  zero, extdp, extwc, zero, zero, zero],
       [43, zero,  zero, extdp, extwc, zero, zero, zero],
       [44, zero,  zero, extdp, extwc, zero, zero, zero],
       [45, zero,  zero, extdp, extwc, zero, zero, zero],
       [46, zero,  zero, extdp, extwc, zero, zero, zero],
       [47, zero,  zero, extdp, extwc, zero, zero, zero],
       [48, zero,  zero, extdp, extwc, zero, zero, zero],
       [49, zero,  zero, extdp, extwc, zero, zero, zero],
       [50, zero,  zero, extdp, extwc, zero, zero, zero],
       [51, zero,  zero, extdp, extwc, zero, zero, zero],
       [52, zero,  zero, extdp, extwc, zero, zero, zero],
       [53, zero,  zero, extdp, extwc, zero, zero, zero],
       [54, zero,  zero, extdp, extwc, zero, zero, zero],
       [55, zero,  zero, extdp, extwc, zero, zero, zero],
       [56, zero,  zero, extdp, extwc, zero, zero, zero],
       [57, zero,  zero, extdp, extwc, zero, zero, zero],
       [58, zero,  zero, extdp, extwc, zero, zero, zero],
       [59, zero,  zero, extdp, extwc, zero, zero, zero],
       [60, zero,  zero, extdp, extwc, zero, zero, zero],
       [61, zero,  zero, extdp, extwc, zero, zero, zero],
       [62, zero,  zero, extdp, extwc, zero, zero, zero],
       [63, zero,  zero, extdp, extwc, zero, zero, zero]],
    
    5:[[ 0, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 1, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 2, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 3, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 4, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 5, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 6, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 7, finf1, pet2, extdp, extwc, zero, zero, zero],
       [ 8, zero,  pet2, extdp, extwc, zero, zero, zero],
       [ 9, zero,  pet2, extdp, extwc, zero, zero, zero],
       [10, zero,  pet2, extdp, extwc, zero, zero, zero],
       [11, zero,  pet2, extdp, extwc, zero, zero, zero],
       [12, zero,  pet2, extdp, extwc, zero, zero, zero],
       [13, zero,  pet2, extdp, extwc, zero, zero, zero],
       [14, zero,  pet2, extdp, extwc, zero, zero, zero],
       [15, zero,  pet2, extdp, extwc, zero, zero, zero],
       [16, zero,  pet2, extdp, extwc, zero, zero, zero],
       [17, zero,  pet2, extdp, extwc, zero, zero, zero],
       [18, zero,  pet2, extdp, extwc, zero, zero, zero],
       [19, zero,  pet2, extdp, extwc, zero, zero, zero],
       [20, zero,  pet2, extdp, extwc, zero, zero, zero],
       [21, zero,  pet2, extdp, extwc, zero, zero, zero],
       [22, zero,  pet2, extdp, extwc, zero, zero, zero],
       [23, zero,  pet2, extdp, extwc, zero, zero, zero],
       [24, zero,  pet2, extdp, extwc, zero, zero, zero],
       [25, zero,  pet2, extdp, extwc, zero, zero, zero],
       [26, zero,  pet2, extdp, extwc, zero, zero, zero],
       [27, zero,  pet2, extdp, extwc, zero, zero, zero],
       [28, zero,  pet2, extdp, extwc, zero, zero, zero],
       [29, zero,  pet2, extdp, extwc, zero, zero, zero],
       [30, zero,  pet2, extdp, extwc, zero, zero, zero],
       [31, zero,  pet2, extdp, extwc, zero, zero, zero],
       [32, zero,  pet2, extdp, extwc, zero, zero, zero],
       [33, zero,  pet2, extdp, extwc, zero, zero, zero],
       [34, zero,  pet2, extdp, extwc, zero, zero, zero],
       [35, zero,  pet2, extdp, extwc, zero, zero, zero],
       [36, zero,  pet2, extdp, extwc, zero, zero, zero],
       [37, zero,  pet2, extdp, extwc, zero, zero, zero],
       [38, zero,  pet2, extdp, extwc, zero, zero, zero],
       [39, zero,  pet2, extdp, extwc, zero, zero, zero],
       [40, zero,  zero, extdp, extwc, zero, zero, zero],
       [41, zero,  zero, extdp, extwc, zero, zero, zero],
       [42, zero,  zero, extdp, extwc, zero, zero, zero],
       [43, zero,  zero, extdp, extwc, zero, zero, zero],
       [44, zero,  zero, extdp, extwc, zero, zero, zero],
       [45, zero,  zero, extdp, extwc, zero, zero, zero],
       [46, zero,  zero, extdp, extwc, zero, zero, zero],
       [47, zero,  zero, extdp, extwc, zero, zero, zero],
       [48, zero,  zero, extdp, extwc, zero, zero, zero],
       [49, zero,  zero, extdp, extwc, zero, zero, zero],
       [50, zero,  zero, extdp, extwc, zero, zero, zero],
       [51, zero,  zero, extdp, extwc, zero, zero, zero],
       [52, zero,  zero, extdp, extwc, zero, zero, zero],
       [53, zero,  zero, extdp, extwc, zero, zero, zero],
       [54, zero,  zero, extdp, extwc, zero, zero, zero],
       [55, zero,  zero, extdp, extwc, zero, zero, zero],
       [56, zero,  zero, extdp, extwc, zero, zero, zero],
       [57, zero,  zero, extdp, extwc, zero, zero, zero],
       [58, zero,  zero, extdp, extwc, zero, zero, zero],
       [59, zero,  zero, extdp, extwc, zero, zero, zero],
       [60, zero,  zero, extdp, extwc, zero, zero, zero],
       [61, zero,  zero, extdp, extwc, zero, zero, zero],
       [62, zero,  zero, extdp, extwc, zero, zero, zero],
       [63, zero,  zero, extdp, extwc, zero, zero, zero]]
}


def get_mf6_model(idx, dir):
    
    tdis_rc = []
    for i in range(nper):
        tdis_rc.append((perlen[i], nstp[i], tsmult[i]))
    
    name = ex[idx]
    
    # build MODFLOW 6 files
    ws = dir
    sim = flopy.mf6.MFSimulation(sim_name=name, version='mf6',
                                 exe_name=mf6_exe,
                                 sim_ws=ws)
    
    # create tdis package
    tdis = flopy.mf6.ModflowTdis(sim, time_units='DAYS',
                                 nper=nper, perioddata=tdis_rc)
    
    # create gwf model
    gwf = flopy.mf6.ModflowGwf(sim, 
                               modelname=name, 
                               newtonoptions=True,
                               save_flows=True)
    
    # create iterative model solution and register the gwf model with it
    ims = flopy.mf6.ModflowIms(sim, print_option='SUMMARY',
                               complexity='MODERATE',
                               outer_dvclose=hclose,
                               outer_maximum=nouter,
                               under_relaxation='DBD',
                               inner_maximum=ninner,
                               inner_dvclose=hclose, rcloserecord=rclose,
                               linear_acceleration='BICGSTAB',
                               scaling_method='NONE',
                               reordering_method='NONE',
                               relaxation_factor=relax)
    sim.register_ims_package(ims, [gwf.name])
    
    dis = flopy.mf6.ModflowGwfdis(gwf, nlay=nlay, nrow=nrow, ncol=ncol,
                                  delr=delr, delc=delc,
                                  top=top, botm=botm)
    
    # initial conditions
    ic = flopy.mf6.ModflowGwfic(gwf, strt=strt)
    
    # node property flow
    npf = flopy.mf6.ModflowGwfnpf(gwf, save_flows=True,
                                  icelltype=icelltype,
                                  k=k,
                                  k33=k33)
    
    # aquifer storage
    sto = flopy.mf6.ModflowGwfsto(gwf, 
                                  iconvert=1, 
                                  ss=ss,
                                  sy=sy,
                                  transient=True)
    
    # ghb files
    ghb = flopy.mf6.ModflowGwfghb(gwf,
                                  print_flows=True, 
                                  stress_period_data=ghbspd)
    
    # transient uzf info
    uzf_obs = {'{}.uzfobs'.format(name):                   # Relies on boundnames
        [('uzf_dpth=0.5', 'water-content', 'uzf01', 0.5),  # Lay 1
         ('uzf_dpth=1.5', 'water-content', 'uzf09', 0.5),  # Lay 2
         ('uzf_dpth=2.5', 'water-content', 'uzf17', 0.5),  # Lay 3
         ('uzf_dpth=3.5', 'water-content', 'uzf25', 0.5),  # Lay 4
         ('uzf_dpth=4.5', 'water-content', 'uzf33', 0.5),  # Lay 5
         ('uzf_dpth=7.5', 'water-content', 'uzf41', 2.5),  # Lay 6
         ('uzf_dpth=15',  'water-content', 'uzf49', 5.0),  # Lay 7
         ('uzf_dpth=25',  'water-content', 'uzf57', 5.0)]} # Lay 8
    
    uzf = flopy.mf6.ModflowGwfuzf(gwf, print_flows=True,
                                  save_flows=True,
                                  wc_filerecord=name + ".uzfwc.bin",
                                  simulate_et=True,
                                  simulate_gwseep=True,
                                  linear_gwet=True,
                                  unsat_etwc=True,
                                  boundnames=True,
                                  observations=uzf_obs,
                                  ntrailwaves=ntrail2,
                                  nwavesets=nsets2,
                                  nuzfcells=len(uzf_pkdat),
                                  packagedata=uzf_pkdat,
                                  perioddata=uzf_spd,
                                  budget_filerecord='{}.uzf.bud'.format(name),
                                  filename='{}.uzf'.format(name))
    
    # output control
    oc = flopy.mf6.ModflowGwfoc(gwf,
                                budget_filerecord='{}.cbc'.format(name),
                                head_filerecord='{}.hds'.format(name),
                                headprintrecord=[
                                    ('COLUMNS', 10, 'WIDTH', 15,
                                     'DIGITS', 6, 'GENERAL')],
                                saverecord=[('HEAD', 'ALL'),
                                            ('BUDGET', 'ALL')],
                                printrecord=[('HEAD', 'ALL'),
                                             ('BUDGET', 'ALL')],
                                filename='{}.oc'.format(name))
    
    return sim

# The MF-NWT code was removed since UZF1 cannot simulate UZET across 
# multiple layers.  In other words, UZF1 truncates extinction depth
# to the first layer's total thickness if the user specifies an extinction
# depth greater than the upper layer's thickness

def build_models():
    for idx, dir in enumerate(exdirs):
        
        # Start by building the MF6 model
        sim = get_mf6_model(idx, dir)
        
        sim.write_simulation()
        
        return sim

def eval_model(sim):
    print('evaluating model...')
    
    name = ex[0]
    ws = exdirs[0]
    sim = flopy.mf6.MFSimulation.load(sim_ws=ws)
    
    # Get the MF6 heads
    fpth = os.path.join(ws, ex[0] + '.hds')
    hobj = flopy.utils.HeadFile(fpth, precision='double')
    hds = hobj.get_alldata()
    
    # Get the MF6 water contents
    wcpth = os.path.join(ws, ex[0] + '.uzfwc.bin')
    mf6_wc_obj = bf.HeadFile(wcpth, text='   water-content')
    
    ckstpkper_wc = mf6_wc_obj.get_kstpkper()

    mf6_wc = []
    for current_kstpkper in ckstpkper_wc:
        wc_rawdat = mf6_wc_obj.get_data(kstpkper = current_kstpkper)
        wc_tmp = np.zeros((nlay, nrow, ncol))
        for i, itm in enumerate(uzf_pkdat):
            lay = itm[1][0]
            row = itm[1][1]
            col = itm[1][2]
            wc_tmp[lay, row, col] = wc_rawdat[0][0][i]
            
        mf6_wc.append(wc_tmp)
    
    mf6_wc = np.array(mf6_wc)

    # First test is that all of the lowest layers have saturated water contents
    # in the final 3 stress periods when the GHB boundary forces submersion of
    # these cells
    assert np.allclose(mf6_wc[-3:-1, -1, 0, 1:-1], thts, atol=0.000001), \
       'Deepest layer water contents not equal to the saturated water content'

    print('Finished running checks')


# - No need to change any code below
def test_mf6model():
    
    # initialize testing framework
    test = testing_framework()

    # build and write the model input
    mf6 = build_models()

    # run the test models
    mf6.run_simulation()
    
    # compare water contents
    eval_model(mf6)

    return


def main():
    
    # initialize testing framework
    test = testing_framework()

    # build the models
    mf6 = build_models()

    # run the test models
    mf6.run_simulation()

    # compare water contents
    eval_model(mf6)

    return


if __name__ == "__main__":
    # print message
    print('standalone run of {}'.format(os.path.basename(__file__)))

    # run main routine
    main()
