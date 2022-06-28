import numpy as np


def write_head(
    fbin,
    data,
    kstp=1,
    kper=1,
    pertim=1.0,
    totim=1.0,
    text="            HEAD",
    ilay=1,
):
    dt = np.dtype(
        [
            ("kstp", np.int32),
            ("kper", np.int32),
            ("pertim", np.float64),
            ("totim", np.float64),
            ("text", "S16"),
            ("ncol", np.int32),
            ("nrow", np.int32),
            ("ilay", np.int32),
        ]
    )
    nrow = data.shape[0]
    ncol = data.shape[1]
    h = np.array((kstp, kper, pertim, totim, text, ncol, nrow, ilay), dtype=dt)
    h.tofile(fbin)
    data.tofile(fbin)
    return


def write_budget(
    fbin,
    data,
    kstp=1,
    kper=1,
    text="    FLOW-JA-FACE",
    imeth=1,
    delt=1.0,
    pertim=1.0,
    totim=1.0,
    text1id1="           GWF-1",
    text2id1="           GWF-1",
    text1id2="           GWF-1",
    text2id2="             NPF",
):
    dt = np.dtype(
        [
            ("kstp", np.int32),
            ("kper", np.int32),
            ("text", "S16"),
            ("ndim1", np.int32),
            ("ndim2", np.int32),
            ("ndim3", np.int32),
            ("imeth", np.int32),
            ("delt", np.float64),
            ("pertim", np.float64),
            ("totim", np.float64),
        ]
    )

    if imeth == 1:
        ndim1 = data.shape[0]
        ndim2 = 1
        ndim3 = -1
        h = np.array(
            (
                kstp,
                kper,
                text,
                ndim1,
                ndim2,
                ndim3,
                imeth,
                delt,
                pertim,
                totim,
            ),
            dtype=dt,
        )
        h.tofile(fbin)
        data.tofile(fbin)

    elif imeth == 6:
        ndim1 = 1
        ndim2 = 1
        ndim3 = -1
        h = np.array(
            (
                kstp,
                kper,
                text,
                ndim1,
                ndim2,
                ndim3,
                imeth,
                delt,
                pertim,
                totim,
            ),
            dtype=dt,
        )
        h.tofile(fbin)

        # write text1id1, ...
        dt = np.dtype(
            [
                ("text1id1", "S16"),
                ("text1id2", "S16"),
                ("text2id1", "S16"),
                ("text2id2", "S16"),
            ]
        )
        h = np.array((text1id1, text1id2, text2id1, text2id2), dtype=dt)
        h.tofile(fbin)

        # write ndat (number of floating point columns)
        colnames = data.dtype.names
        ndat = len(colnames) - 2
        dt = np.dtype([("ndat", np.int32)])
        h = np.array([(ndat,)], dtype=dt)
        h.tofile(fbin)

        # write auxiliary column names
        naux = ndat - 1
        if naux > 0:
            auxtxt = [f"{colname:16}" for colname in colnames[3:]]
            auxtxt = tuple(auxtxt)
            dt = np.dtype([(colname, "S16") for colname in colnames[3:]])
            h = np.array(auxtxt, dtype=dt)
            h.tofile(fbin)

        # write nlist
        nlist = data.shape[0]
        dt = np.dtype([("nlist", np.int32)])
        h = np.array([(nlist,)], dtype=dt)
        h.tofile(fbin)

        # write the data
        data.tofile(fbin)

        pass
    else:
        raise Exception(f"unknown method code {imeth}")
    return


def uniform_flow_field(qx, qy, qz, shape, delr=None, delc=None, delv=None):

    nlay, nrow, ncol = shape

    # create spdis array for the uniform flow field
    dt = np.dtype(
        [
            ("ID1", np.int32),
            ("ID2", np.int32),
            ("FLOW", np.float64),
            ("QX", np.float64),
            ("QY", np.float64),
            ("QZ", np.float64),
        ]
    )
    spdis = np.array(
        [(id1, id1, 0.0, qx, qy, qz) for id1 in range(nlay * nrow * ncol)],
        dtype=dt,
    )

    # create the flowja array for the uniform flow field (assume top-bot = 1)
    flowja = []
    if delr is None:
        delr = 1.0
    if delc is None:
        delc = 1.0
    if delv is None:
        delv = 1.0
    for k in range(nlay):
        for i in range(nrow):
            for j in range(ncol):
                # diagonal
                flowja.append(0.0)
                # up
                if k > 0:
                    flowja.append(-qz * delr * delc)
                # back
                if i > 0:
                    flowja.append(-qy * delr * delv)
                # left
                if j > 0:
                    flowja.append(qx * delc * delv)
                # right
                if j < ncol - 1:
                    flowja.append(-qx * delc * delv)
                # front
                if i < nrow - 1:
                    flowja.append(qy * delr * delv)
                # bottom
                if k < nlay - 1:
                    flowja.append(qz * delr * delc)
    flowja = np.array(flowja, dtype=np.float64)
    return spdis, flowja
