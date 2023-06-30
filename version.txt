# MODFLOW 6 version file automatically created using...update_version.py
# created on...June 29, 2023 00:01:07

major = 6
minor = 4
micro = 2
label = '+'
__version__ = '{:d}.{:d}.{:d}'.format(major, minor, micro)
if label:
	__version__ += '{}{}'.format(__version__, label)