idmloader_sources = files(
    'src' / 'idmloader.f90',
)

executable('idmloader',
           idmloader_sources,
           link_with: mf6core, 
           install: true,
           fortran_args: util_args)
