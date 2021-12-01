zonebudget_sources = files(
    'src' / 'budgetdata.f90',
    'src' / 'grb.f90',
    'src' / 'zbud6.f90',
    'src' / 'zone.f90',
    'src' / 'zoneoutput.f90',
)

executable('zbud6',
           zonebudget_sources,
           link_with: mf6core, 
           install: true,
           fortran_args: util_args)
