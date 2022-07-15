# utility for comparing two MODFLOW 6 budget files

# To use this eval_bud_diff function on a gwf or gwt budget file,
# the function may need ia, in order to exclude comparison of the residual
# term, which is stored in the diagonal position of the flowja array.
#  The following code can be used to extract ia from the grb file.
# get ia/ja from binary grid file
# fname = '{}.dis.grb'.format(os.path.basename(sim.name))
# fpth = os.path.join(sim.simpath, fname)
# grbobj = flopy.mf6.utils.MfGrdFile(fpth)
# ia = grbobj._datadict['IA'] - 1


import os

import numpy as np


def eval_bud_diff(fpth, b0, b1, ia=None, dtol=1e-6):
    diffmax = 0.0
    difftag = "None"
    difftime = None
    fail = False

    # build list of cbc data to retrieve
    avail = b0.get_unique_record_names()

    # initialize list for storing totals for each budget term terms
    cbc_keys = []
    for t in avail:
        if isinstance(t, bytes):
            t = t.decode()
        t = t.strip()
        cbc_keys.append(t)

    # open a summary file and write header
    f = open(fpth, "w")
    line = f"{'Time':15s}"
    line += f" {'Datatype':15s}"
    line += f" {'File 1':15s}"
    line += f" {'File 2':15s}"
    line += f" {'Difference':15s}"
    f.write(line + "\n")
    f.write(len(line) * "-" + "\n")

    # get data from cbc file
    kk = b0.get_kstpkper()
    times = b0.get_times()
    for idx, (k, t) in enumerate(zip(kk, times)):
        v0sum = 0.0
        v1sum = 0.0
        for key in cbc_keys:
            v0 = b0.get_data(kstpkper=k, text=key)[0]
            v1 = b1.get_data(kstpkper=k, text=key)[0]
            if isinstance(v0, np.recarray):
                v0 = v0["q"].sum()
                v1 = v1["q"].sum()
            else:
                v0 = v0.flatten()
                v1 = v1.flatten()
                if key == "FLOW-JA-FACE":
                    # Set residual (stored in diagonal of flowja) to zero
                    if ia is None:
                        raise Exception("ia is required for model flowja")
                    idiagidx = ia[:-1]
                    v0[idiagidx] = 0.0
                    v1[idiagidx] = 0.0
                v0 = v0.sum()
                v1 = v1.sum()

            # sum all of the values
            if key != "AUXILIARY":
                v0sum += v0
                v1sum += v1

            diff = v0 - v1
            if abs(diff) > abs(diffmax):
                diffmax = diff
                difftag = key
                difftime = t
            if abs(diff) > dtol:
                fail = True
            line = f"{t:15g}"
            line += f" {key:15s}"
            line += f" {v0:15g}"
            line += f" {v1:15g}"
            line += f" {diff:15g}"
            f.write(line + "\n")

    # evaluate the sums
    diff = v0sum - v1sum
    if abs(diff) > dtol:
        fail = True
    line = f"{t:15g}"
    line += f" {'TOTAL':15s}"
    line += f" {v0sum:15g}"
    line += f" {v1sum:15g}"
    line += f" {diff:15g}"
    f.write(line + "\n")

    msg = f"\nSummary of changes in {os.path.basename(fpth)}\n"
    msg += "-" * 72 + "\n"
    msg += f"Maximum cbc difference:        {diffmax}\n"
    msg += f"Maximum cbc difference time:   {difftime}\n"
    msg += f"Maximum cbc datatype:          {difftag}\n"
    if fail:
        msg += f"Maximum cbc criteria exceeded:  {dtol}"
    assert not fail, msg

    # close summary file and print the final message
    f.close()
    print(msg)

    msg = f"sum of first cbc file flows ({v0sum}) " + f"exceeds dtol ({dtol})"
    assert abs(v0sum) < dtol, msg

    msg = f"sum of second cbc file flows ({v1sum}) " + f"exceeds dtol ({dtol})"
    assert abs(v1sum) < dtol, msg

    return
