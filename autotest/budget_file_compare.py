# utility for comparing two MODFLOW 6 budget files

# To use this eval_bud_diff function on a gwf or gwt budget file,
# the function may need ia, in order to exclude comparison of the residual
# term, which is stored in the diagonal position of the flowja array.
#  The following code can be used to extract ia from the grb file.
# get ia/ja from binary grid file
# fname = '{}.dis.grb'.format(os.path.basename(sim.name))
# fpth = os.path.join(sim.simpath, fname)
# grbobj = flopy.utils.MfGrdFile(fpth)
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
    line = "{:15s}".format("Time")
    line += " {:15s}".format("Datatype")
    line += " {:15s}".format("Variables")
    line += " {:15s}".format("Timeseries")
    line += " {:15s}".format("Difference")
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
            line = "{:15g}".format(t)
            line += " {:15s}".format(key)
            line += " {:15g}".format(v0)
            line += " {:15g}".format(v1)
            line += " {:15g}".format(diff)
            f.write(line + "\n")

    # evaluate the sums
    diff = v0sum - v1sum
    if abs(diff) > dtol:
        fail = True
    line = "{:15g}".format(t)
    line += " {:15s}".format("TOTAL")
    line += " {:15g}".format(v0sum)
    line += " {:15g}".format(v1sum)
    line += " {:15g}".format(diff)
    f.write(line + "\n")

    msg = "\nSummary of changes in {}\n".format(os.path.basename(fpth))
    msg += "-" * 72 + "\n"
    msg += "Maximum cbc difference:        {}\n".format(diffmax)
    msg += "Maximum cbc difference time:   {}\n".format(difftime)
    msg += "Maximum cbc datatype:          {}\n".format(difftag)
    if fail:
        msg += "Maximum cbc criteria exceeded:  {}".format(dtol)
    assert not fail, msg

    # close summary file and print the final message
    f.close()
    print(msg)

    msg = "sum of first cbc file flows ({}) ".format(
        v0sum
    ) + "exceeds dtol ({})".format(dtol)
    assert abs(v0sum) < dtol, msg

    msg = "sum of second cbc file flows ({}) ".format(
        v1sum
    ) + "exceeds dtol ({})".format(dtol)
    assert abs(v1sum) < dtol, msg

    return
