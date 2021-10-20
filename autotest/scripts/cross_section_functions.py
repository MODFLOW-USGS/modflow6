import numpy as np

# n-point cross-section functions
def get_wetted_station(
    x0,
    x1,
    d0,
    d1,
    d,
):
    """Get the wetted length in the x-direction"""
    # -- calculate the minimum and maximum depth
    dmin = min(d0, d1)
    dmax = max(d0, d1)

    # -- if d is less than or equal to the minimum value the
    #    station length (xlen) is zero
    if d <= dmin:
        x1 = x0
    # -- if d is between dmin and dmax, station length is less
    #    than d1 - d0
    elif d < dmax:
        xlen = x1 - x0
        dlen = d1 - d0
        if abs(dlen) > 0.0:
            slope = xlen / dlen
        else:
            slope = 0.0
        if d0 > d1:
            dx = (d - d1) * slope
            xt = x1 + dx
            xt0 = xt
            xt1 = x1
        else:
            dx = (d - d0) * slope
            xt = x0 + dx
            xt0 = x0
            xt1 = xt
        x0 = xt0
        x1 = xt1
    return x0, x1


def get_wetted_perimeter(
    x0,
    x1,
    d0,
    d1,
    d,
):
    # -- calculate the minimum and maximum depth
    dmin = min(d0, d1)
    dmax = max(d0, d1)

    # -- calculate the wetted perimeter for the segment
    xlen = x1 - x0
    if xlen > 0.0:
        if d > dmax:
            dlen = dmax - dmin
        else:
            dlen = d - dmin
    else:
        if d > dmin:
            dlen = min(d, dmax) - dmin
        else:
            dlen = 0.0
    return np.sqrt(xlen ** 2.0 + dlen ** 2.0)


def get_wetted_area(x0, x1, d0, d1, d):
    # -- calculate the minimum and maximum depth
    dmin = min(d0, d1)
    dmax = max(d0, d1)

    # -- calculate the wetted area for the segment
    xlen = x1 - x0
    area = 0.0
    if xlen > 0.0:
        # -- add the area above dmax
        if d > dmax:
            area = xlen * (d - dmax)
        # -- add the area below zmax
        if dmax != dmin and d > dmin:
            area += 0.5 * (d - dmin)
    return area


def wetted_area(
    x,
    d,
    v,
    verbose=False,
):
    area = 0.0
    if x.shape[0] == 1:
        area = x[0] * v
    else:
        for idx in range(0, x.shape[0] - 1):
            x0, x1 = x[idx], x[idx + 1]
            d0, d1 = d[idx], d[idx + 1]

            # get station data
            x0, x1 = get_wetted_station(x0, x1, d0, d1, v)

            # get wetted area
            a = get_wetted_area(x0, x1, d0, d1, v)
            area += a

            # write to screen
            if verbose:
                print(
                    f"{idx}->{idx + 1} ({x0},{x1}) - "
                    f"perimeter={x1 - x0} - area={a}"
                )

    return area


def wetted_perimeter(
    x,
    d,
    v,
    verbose=False,
):
    perimeter = 0.0
    if x.shape[0] == 1:
        perimeter = x[0]
    else:
        for idx in range(0, x.shape[0] - 1):
            x0, x1 = x[idx], x[idx + 1]
            d0, d1 = d[idx], d[idx + 1]

            # get station data
            x0, x1 = get_wetted_station(x0, x1, d0, d1, v)

            # get wetted perimeter
            perimeter += get_wetted_perimeter(x0, x1, d0, d1, v)

            # write to screen
            if verbose:
                print(f"{idx}->{idx + 1} ({x0},{x1}) - perimeter={x1 - x0}")

    return perimeter


def manningsq(
    x,
    d,
    v,
    roughness=0.01,
    slope=0.001,
    conv=1.0,
):
    if isinstance(roughness, float):
        roughness = np.ones(x.shape, dtype=float) * roughness
    if x.shape[0] > 1:
        f = 0.0
        for i0 in range(x.shape[0] - 1):
            i1 = i0 + 1
            perimeter = get_wetted_perimeter(x[i0], x[i1], d[i0], d[i1], v)
            area = get_wetted_area(x[i0], x[i1], d[i0], d[i1], v)
            if perimeter > 0.0:
                radius = area / perimeter
                f += area * radius ** 0.666666 / roughness[i0]
    else:
        perimeter = wetted_perimeter(x, d, v)
        area = wetted_area(x, d, v)
        radius = 0.0
        if perimeter > 0.0:
            radius = area / perimeter
        f = area * radius ** 0.666666 / roughness[0]
    return conv * slope ** 0.5 * f


def get_depths(
    flows,
    x,
    d,
    roughness=0.01,
    slope=0.001,
    conv=1.0,
    dd=1e-4,
    verbose=False,
):
    if isinstance(flows, float):
        flows = np.array([flows], dtype=float)
    if isinstance(roughness, float):
        roughness = np.ones(x.shape, dtype=float) * roughness
    depths = np.zeros(flows.shape, dtype=float)
    for idx, q in enumerate(flows):
        depths[idx] = qtodepth(
            x,
            d,
            q,
            roughness=roughness,
            slope=slope,
            conv=conv,
            dd=dd,
            verbose=False,
        )

    return depths


def qtodepth(
    x,
    d,
    q,
    roughness=0.01,
    slope=0.001,
    conv=1.0,
    dd=1e-4,
    verbose=False,
):
    d0 = 0.0
    q0 = manningsq(
        x,
        d,
        d0,
        roughness=roughness,
        slope=slope,
        conv=conv,
    )
    r = q0 - q

    iter = 0
    if verbose:
        print(f"iteration {iter:>2d} - residual={r}")
    while abs(r) > 1e-12:
        q1 = manningsq(
            x,
            d,
            d0 + dd,
            roughness=roughness,
            slope=slope,
            conv=conv,
        )
        dq = q1 - q0
        if dq != 0.0:
            derv = dd / (q1 - q0)
        else:
            derv = 0.0
        d0 -= derv * r
        q0 = manningsq(
            x,
            d,
            d0,
            roughness=roughness,
            slope=slope,
            conv=conv,
        )
        r = q0 - q

        iter += 1
        if verbose:
            print(f"iteration {iter:>2d} - residual={r}")
        if iter > 100:
            break
    return d0
