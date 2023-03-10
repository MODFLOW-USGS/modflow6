project(
  'MODFLOW 6',
  'fortran',
  version: '6.5.0',
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

do_parallel_build = get_option('parallel')
message('The used profile is:', profile)
message('Parallel build:', do_parallel_build)

# windows options:
petsc_dir = 'd:\checkouts\petsc-3.18.5'
petsc_arch = 'arch-ci-mswin-intel-modflow6'

fc = meson.get_compiler('fortran')
fc_id = fc.get_id()
compile_args = []
link_args = []

# Command line options for gfortran
if fc_id == 'gcc'
  # General options
  compile_args += [
                   '-fall-intrinsics',
                   '-pedantic',
                   '-cpp',
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
  system = build_machine.system()
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
                   '/heap-arrays:0',
                   '/traceback',
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
                   '-no-heap-arrays',
                   '-traceback',
                   '-diag-disable:7416',  # f2008 warning
                   '-diag-disable:7025',  # f2008 warning
                   '-diag-disable:5268',  # Line too long
                  ]
  link_args += '-static-intel'
endif

if build_machine.system() == 'windows'
  # directly look for petsc
  petsc_compiled = petsc_dir / petsc_arch
  petsc = fc.find_library('libpetsc', dirs: petsc_compiled / 'lib')
else
  petsc = dependency('PETSc', required : false)
endif
mpi = dependency('mpi', language : 'fortran', required : petsc.found())

# on windows only with intel
enable_mpi = do_parallel_build
if build_machine.system() == 'windows' and do_parallel_build
  if fc_id == 'intel-cl'
    enable_mpi = true
  else
    message('Parallel build on Windows only with intel compiler, disabling...')
    enable_mpi
  endif
endif

# compile with mpi or petsc+mpi when found,
# (not allowing petsc without mpi)
dependencies = [ ]
extra_cmp_args = [ ]

if mpi.found() and enable_mpi
  extra_cmp_args = [ '-D__WITH_MPI__' ]
  with_mpi = true
  dependencies = [ mpi ]
else
  with_mpi = false
endif 

if petsc.found() and with_mpi
  extra_cmp_args = [ '-D__WITH_MPI__', '-D__WITH_PETSC__' ]
  with_petsc = true
  dependencies = [ mpi, petsc ]
else
  with_petsc = false
endif

compile_args += extra_cmp_args

add_project_arguments(fc.get_supported_arguments(compile_args), language: 'fortran')
add_project_link_arguments(fc.get_supported_arguments(link_args), language: 'fortran')

if with_petsc and build_machine.system() == 'windows'
  message('Compiling PETSc Fortran modudules')
  petsc_incdir = include_directories([petsc_dir / 'include', petsc_compiled / 'include'])
  petsc_src = petsc_dir / 'src'
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
endif

subdir('src')
subdir('srcbmi')
subdir('utils')

# add unit test directory
# subdir('unittests')

# meson tests to evaluate installation success
test('Test installation version', mf6exe, args : ['-v',])
test('Test installation compiler', mf6exe, args : ['-c',])
test('Test installation help', mf6exe, args : ['-h',])
