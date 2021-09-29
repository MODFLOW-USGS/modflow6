project('Modflow 6', 'fortran',
  version : '6.3.0', # TODO: only meson should manage the version number
  meson_version: '>= 0.59.0',
  default_options : ['buildtype=debugoptimized'])

fc = meson.get_compiler('fortran')

if fc.get_id() == 'intel-cl' # windows
  add_project_arguments(
    fc.get_supported_arguments([
      '/fpp',
      '/static'
     ]),
     language: 'fortran'
  )
elif fc.get_id() == 'intel' # linux and mac
  add_project_arguments(
    fc.get_supported_arguments([
      '-fpp',
      '-static'
     ]),
     language: 'fortran'
  )
endif

mpi = dependency('mpi', language : 'fortran', required : false)

subdir('src')
subdir('srcbmi')
subdir('utils')
