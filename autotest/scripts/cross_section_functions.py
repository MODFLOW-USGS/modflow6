import numpy as np

# power for Manning's hydraulic radius term
mpow = 2.0 / 3.0


def calculate_rectchan_mannings_discharge(
    conversion_factor, roughness, slope, width, depth
):
    """
    Calculate Manning's discharge for a rectangular channel.

    """
    area = width * depth
    return conversion_factor * area * depth**mpow * slope**0.5 / roughness


# n-point cross-section functions
def get_wetted_station(
    x0,
    x1,
    h0,
    h1,
    depth,
):
    """Get the wetted length in the x-direction"""
    # -- calculate the minimum and maximum depth
    hmin = min(h0, h1)
    hmax = max(h0, h1)

    # -- if depth is less than or equal to the minimum value the
    #    station length (xlen) is zero
    if depth <= hmin:
        x1 = x0
    # -- if depth is between hmin and hmax, station length is less
    #    than h1 - h0
    elif depth < hmax:
        xlen = x1 - x0
        dlen = h1 - h0
        if abs(dlen) > 0.0:
            slope = xlen / dlen
        else:
            slope = 0.0
        if h0 > h1:
            dx = (depth - h1) * slope
            xt = x1 + dx
            xt0 = xt
            xt1 = x1
        else:
            dx = (depth - h0) * slope
            xt = x0 + dx
            xt0 = x0
            xt1 = xt
        x0 = xt0
        x1 = xt1
    return x0, x1


def get_wetted_perimeter(
    x0,
    x1,
    h0,
    h1,
    depth,
):
    # -- calculate the minimum and maximum depth
    hmin = min(h0, h1)
    hmax = max(h0, h1)

    # -- calculate the wetted perimeter for the segment
    xlen = x1 - x0
    if xlen > 0.0:
        if depth > hmax:
            dlen = hmax - hmin
        else:
            dlen = depth - hmin
    else:
        if depth > hmin:
            dlen = min(depth, hmax) - hmin
        else:
            dlen = 0.0
    return np.sqrt(xlen**2.0 + dlen**2.0)


def get_wetted_area(x0, x1, h0, h1, depth):
    # -- calculate the minimum and maximum depth
    hmin = min(h0, h1)
    hmax = max(h0, h1)

    # -- calculate the wetted area for the segment
    xlen = x1 - x0
    area = 0.0
    if xlen > 0.0:
        # -- add the area above hmax
        if depth > hmax:
            area = xlen * (depth - hmax)
        # -- add the area below zmax
        if hmax != hmin and depth > hmin:
            area += 0.5 * (depth - hmin)
    return area


def wetted_area(
    x,
    h,
    depth,
    verbose=False,
):
    area = 0.0
    if x.shape[0] == 1:
        area = x[0] * depth
    else:
        for idx in range(0, x.shape[0] - 1):
            x0, x1 = x[idx], x[idx + 1]
            h0, h1 = h[idx], h[idx + 1]

            # get station data
            x0, x1 = get_wetted_station(x0, x1, h0, h1, depth)

            # get wetted area
            a = get_wetted_area(x0, x1, h0, h1, depth)
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
    h,
    depth,
    verbose=False,
):
    perimeter = 0.0
    if x.shape[0] == 1:
        perimeter = x[0]
    else:
        for idx in range(0, x.shape[0] - 1):
            x0, x1 = x[idx], x[idx + 1]
            h0, h1 = h[idx], h[idx + 1]

            # get station data
            x0, x1 = get_wetted_station(x0, x1, h0, h1, depth)

            # get wetted perimeter
            perimeter += get_wetted_perimeter(x0, x1, h0, h1, depth)

            # write to screen
            if verbose:
                print(f"{idx}->{idx + 1} ({x0},{x1}) - perimeter={x1 - x0}")

    return perimeter


def manningsq(
    x,
    h,
    depth,
    roughness=0.01,
    slope=0.001,
    conv=1.0,
):
    if isinstance(roughness, float):
        roughness = np.ones(x.shape, dtype=float) * roughness
    if x.shape[0] > 1:
        q = 0.0
        for i0 in range(x.shape[0] - 1):
            i1 = i0 + 1
            perimeter = get_wetted_perimeter(x[i0], x[i1], h[i0], h[i1], depth)
            area = get_wetted_area(x[i0], x[i1], h[i0], h[i1], depth)
            if perimeter > 0.0:
                radius = area / perimeter
                q += (
                    conv * area * radius**mpow * slope**0.5 / roughness[i0]
                )
    else:
        perimeter = wetted_perimeter(x, h, depth)
        area = wetted_area(x, h, depth)
        radius = 0.0
        if perimeter > 0.0:
            radius = area / perimeter
        q = conv * area * radius**mpow * slope**0.5 / roughness[0]
    return q


def get_depths(
    flows,
    x,
    h,
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
            h,
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
    h,
    q,
    roughness=0.01,
    slope=0.001,
    conv=1.0,
    dd=1e-4,
    verbose=False,
):
    h0 = 0.0
    q0 = manningsq(
        x,
        h,
        h0,
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
            h,
            h0 + dd,
            roughness=roughness,
            slope=slope,
            conv=conv,
        )
        dq = q1 - q0
        if dq != 0.0:
            derv = dd / (q1 - q0)
        else:
            derv = 0.0
        h0 -= derv * r
        q0 = manningsq(
            x,
            h,
            h0,
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
    return h0
