project('Modflow 6', 'fortran',
  version : '0.0.1', # TODO
  meson_version: '>= 0.59.0')

fc = meson.get_compiler('fortran')

if fc.get_id() == 'gcc'
  add_project_arguments(
    fc.get_supported_arguments([
      '-Wno-unused-dummy-argument',
      '-Wno-conversion',
      '-Wno-intrinsic-shadow',
      '-Wno-maybe-uninitialized',
      '-w' # TODO: this removes all warnings, remove this line before merging
    ]),
    language : 'fortran'
  )
elif fc.get_id() == 'intel-cl'
  add_project_arguments(
    fc.get_supported_arguments([
      '/fpp',
     ]),
     language: 'fortran'
  )
endif

subdir('src')
subdir('srcbmi')
