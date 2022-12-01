import numpy as np


def get_disu_kwargs(nlay, nrow, ncol, delr, delc, tp, botm):
    """
    Simple utility for creating args needed to construct
    a disu package

    """

    def get_nn(k, i, j):
        return k * nrow * ncol + i * ncol + j

    nodes = nlay * nrow * ncol
    iac = np.zeros((nodes), dtype=int)
    ja = []
    area = np.zeros((nodes), dtype=float)
    top = np.zeros((nodes), dtype=float)
    bot = np.zeros((nodes), dtype=float)
    ihc = []
    cl12 = []
    hwva = []
    for k in range(nlay):
        for i in range(nrow):
            for j in range(ncol):
                # diagonal
                n = get_nn(k, i, j)
                ja.append(n)
                iac[n] += 1
                area[n] = delr[i] * delc[j]
                ihc.append(n + 1)
                cl12.append(n + 1)
                hwva.append(n + 1)
                if k == 0:
                    top[n] = tp
                else:
                    top[n] = botm[k - 1]
                bot[n] = botm[k]
                # up
                if k > 0:
                    ja.append(get_nn(k - 1, i, j))
                    iac[n] += 1
                    ihc.append(0)
                    dz = botm[k - 1] - botm[k]
                    cl12.append(0.5 * dz)
                    hwva.append(delr[i] * delc[j])
                # back
                if i > 0:
                    ja.append(get_nn(k, i - 1, j))
                    iac[n] += 1
                    ihc.append(1)
                    cl12.append(0.5 * delc[i])
                    hwva.append(delr[j])
                # left
                if j > 0:
                    ja.append(get_nn(k, i, j - 1))
                    iac[n] += 1
                    ihc.append(1)
                    cl12.append(0.5 * delr[j])
                    hwva.append(delc[i])
                # right
                if j < ncol - 1:
                    ja.append(get_nn(k, i, j + 1))
                    iac[n] += 1
                    ihc.append(1)
                    cl12.append(0.5 * delr[j])
                    hwva.append(delc[i])
                # front
                if i < nrow - 1:
                    ja.append(get_nn(k, i + 1, j))
                    iac[n] += 1
                    ihc.append(1)
                    cl12.append(0.5 * delc[i])
                    hwva.append(delr[j])
                # bottom
                if k < nlay - 1:
                    ja.append(get_nn(k + 1, i, j))
                    iac[n] += 1
                    ihc.append(0)
                    if k == 0:
                        dz = tp - botm[k]
                    else:
                        dz = botm[k - 1] - botm[k]
                    cl12.append(0.5 * dz)
                    hwva.append(delr[i] * delc[j])
    ja = np.array(ja, dtype=int)
    nja = ja.shape[0]
    hwva = np.array(hwva, dtype=float)
    kw = {}
    kw["nodes"] = nodes
    kw["nja"] = nja
    kw["nvert"] = None
    kw["top"] = top
    kw["bot"] = bot
    kw["area"] = area
    kw["iac"] = iac
    kw["ja"] = ja
    kw["ihc"] = ihc
    kw["cl12"] = cl12
    kw["hwva"] = hwva
    return kw
