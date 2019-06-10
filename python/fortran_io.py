"""Read and write Fortran values from MF6.
"""

import mf6


# Put this into a file?

MF6_DATA_TYPE_TABLE = {
    ('NPER', 'TDIS'): {'data_type': 'int_scalar'},
    ('DELT', 'TDIS'): {'data_type': 'float_scalar'},
    ('NSTP', 'TDIS'): {'data_type': 'int_1d',
                       'dim': ('NPER', 'TDIS')},
    ('PERLEN', 'TDIS'): {'data_type': 'float_1d',
                         'dim': ('NPER', 'TDIS')},
    ('LRCH', 'SLN_1'): {'data_type': 'int_2d',
                        'dim': (3, ('MXITER', 'SLN_1'))},
    ('MXITER', 'SLN_1'): {'data_type': 'int_scalar'},
    ('AUXVAR', 'GWF_1 WEL'): {'data_type': 'float_2d',
                              'dim': (('NAUX', 'GWF_1 WEL'),
                                      ('MAXBOUND', 'GWF_1 WEL'))},
    ('NAUX', 'GWF_1 WEL'): {'data_type': 'int_scalar'},
    ('MAXBOUND', 'GWF_1 WEL'): {'data_type': 'int_scalar'},
}


def get_int(name, origin):
    """Get an integer scalar.
    """
    mf6.access_memory.get_int(name, origin)
    return mf6.shared_data.int_scalar


def get_float(name, origin):
    """Get an float scalar.
    """
    mf6.access_memory.get_float(name, origin)
    return mf6.shared_data.float_scalar


def get_int_1d(name, origin, dim):
    """Get a 1d integer array.
    """
    mf6.access_memory.get_int_1d(name, origin, dim)
    return mf6.shared_data.int_1d


def get_float_1d(name, origin, dim):
    """Get a 1d float array.
    """
    mf6.access_memory.get_float_1d(name, origin, dim)
    return mf6.shared_data.float_1d


def get_int_2d(name, origin, ncol, nrow):
    """Get a 2d integer array.
    """
    mf6.access_memory.get_int_2d(name, origin, ncol, nrow)
    return mf6.shared_data.int_2d


def get_float_2d(name, origin, ncol, nrow):
    """Get a 2d float array.
    """
    mf6.access_memory.get_float_2d(name, origin, ncol, nrow)
    return mf6.shared_data.float_2d


READING_FUNCTIONS = {
    'int_scalar': get_int,
    'float_scalar': get_float,
    'int_1d': get_int_1d,
    'float_1d': get_float_1d,
    'int_2d': get_int_2d,
    'float_2d': get_float_2d,
    }


WRITING_FUNCTIONS = {
    'int_scalar': mf6.access_memory.set_int,
    'float_scalar': mf6.access_memory.set_float,
    'int_1d': mf6.access_memory.set_int_1d,
    'float_1d': mf6.access_memory.set_float_1d,
    'int_2d': mf6.access_memory.set_int_2d,
    'float_2d': mf6.access_memory.set_float_2d,
    }


def get_value(name, origin):
    """Get the value for any dimension and data type.
    """

    def dim2int(dim):
        """Convert `dim` to an integer is is not yet.
        """
        if isinstance(dim, int):
            return dim
        return get_value(*dim)

    entry = MF6_DATA_TYPE_TABLE[(name, origin)]
    data_type = entry['data_type']
    func = READING_FUNCTIONS[data_type]
    dimensionality = data_type.split('_')[1]
    if dimensionality == 'scalar':  # pylint: disable=no-else-return
        return func(name, origin)
    elif dimensionality == '1d':
        dim = dim2int(entry['dim'])
        return func(name, origin, dim)
    elif dimensionality == '2d':
        ncol, nrow = [dim2int(x) for x in entry['dim']]
        return func(name, origin, ncol, nrow)
    else:
        raise TypeError(f'Data type {data_type} not supported.')


def set_value(name, origin, value):
    """Set the value of any dimension and data type.
    """
    entry = MF6_DATA_TYPE_TABLE[(name, origin)]
    data_type = entry['data_type']
    func = WRITING_FUNCTIONS[data_type]
    func(name, origin, value)
