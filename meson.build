project(
  'MODFLOW 6',
  'fortran',
  version: '6.3.0',
  meson_version: '>= 0.59.0',
  default_options : [
    'b_vscrt=static_from_buildtype', # Link runtime libraries statically on Windows
    'optimization=2',
    'fortran_std=f2008',
])

if get_option('optimization') == '3'
  error('Only optimization levels <= 2 are supported')
endif

if get_option('optimization') == '2'
  profile = 'release'
else
  profile = 'develop'
endif

message('The used profile is:', profile)


fc = meson.get_compiler('fortran')
fc_id = fc.get_id()
compile_args = []
link_args = []

system = build_machine.system()

# Command line options for gfortran
if fc_id == 'gcc'
  # General options
  compile_args += ['-pedantic',
                   '-cpp',                       # Enable preprocessing
                   '-Wcharacter-truncation',
                   '-Wno-unused-dummy-argument', # This makes problems with OOP
                   '-Wno-intrinsic-shadow',      # We shadow intrinsics with methods, which should be fine
                   '-Wno-maybe-uninitialized',   # "Uninitialized" flags produce false positives with allocatables
                   '-Wno-uninitialized',
                   ]

  # Options specific to profile
  if profile == 'release'
    compile_args += ['-ffpe-summary=overflow', '-ffpe-trap=overflow,zero,invalid']
  elif profile == 'develop'
    compile_args += ['-fcheck=all', '-ffpe-trap=overflow,zero,invalid,denormal']
  endif

  # Define OS with gfortran for OS specific code
  # These are identical to pre-defined macros available with ifort
  if system == 'linux'
    compile_args += '-D__linux__'
  elif system == 'darwin'
    compile_args += '-D__APPLE__'
  elif system == 'windows'
    compile_args += '-D_WIN32'
  endif
endif

# Command line options for ifort
if fc_id == 'intel-cl'
  # windows
  compile_args += ['/fpe:0',              # Activate all floating point exceptions
                   '/fpp',                # Activate preprocessing
                   '/Qdiag-disable:7416', # f2008 warning
                   '/Qdiag-disable:7025', # f2008 warning
                   '/Qdiag-disable:5268', # Line too long
                  ]
  link_args += ['/ignore:4217', # access through ddlimport might be inefficient
                '/ignore:4286'  # same as 4217, but more general
               ]
elif fc_id == 'intel'
  # linux and macOS
  compile_args += ['-fpe0',              # Activate all floating point exceptions
                  '-diag-disable:7416',  # f2008 warning
                  '-diag-disable:7025',  # f2008 warning
                  '-diag-disable:5268',  # Line too long
                  ]
  link_args += '-static-intel'
endif

add_project_arguments(fc.get_supported_arguments(compile_args), language: 'fortran')
add_project_link_arguments(fc.get_supported_arguments(link_args), language: 'fortran')

mpi = dependency('mpi', language : 'fortran')
if system == 'windows'
  petsc_path= meson.project_source_root() / '..' / 'petsc_test_windows' / 'petsc-3.14.6'
  petsc_compiled = petsc_path / 'arch-mswin-c-opt'
  petsc = fc.find_library('libpetsc', dirs: petsc_compiled / 'lib')
  dependencies = [mpi, petsc]

  petsc_incdir = include_directories([petsc_path / 'include',
                                    petsc_compiled / 'include'])
  petsc_src = petsc_path / 'src'
  sources_petsc = [petsc_src / 'dm/f90-mod/petscdmdamod.F90',
                  petsc_src / 'dm/f90-mod/petscdmmod.F90',
                  petsc_src / 'dm/f90-mod/petscdmplexmod.F90',
                  petsc_src / 'dm/f90-mod/petscdmswarmmod.F90',
                  petsc_src / 'ksp/f90-mod/petsckspdefmod.F90',
                  petsc_src / 'ksp/f90-mod/petsckspmod.F90',
                  petsc_src / 'ksp/f90-mod/petscpcmod.F90',
                  petsc_src / 'mat/f90-mod/petscmatmod.F90',
                  petsc_src / 'snes/f90-mod/petscsnesmod.F90',
                  petsc_src / 'sys/f90-mod/petscsysmod.F90',
                  petsc_src / 'sys/mpiuni/f90-mod/mpiunimod.F90',
                  petsc_src / 'tao/f90-mod/petsctaomod.F90',
                  petsc_src / 'ts/f90-mod/petsctsmod.F90',
                  petsc_src / 'vec/f90-mod/petscvecmod.F90',]
  petsc_modules = static_library('petsc_modules', 
                                sources_petsc,
                                dependencies: dependencies,
                                include_directories: petsc_incdir)
else
  petsc = dependency('PETSc')
  dependencies = [mpi, petsc]
endif

subdir('src')
subdir('srcbmi')
subdir('utils')

# add unit test directory
# subdir('unittests')
