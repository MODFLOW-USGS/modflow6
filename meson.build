project(
  'MODFLOW 6',
  'fortran',
  version: '6.4.4',
  license: 'CC0',
  meson_version: '>= 1.1.0',
  default_options : [
    'b_vscrt=static_from_buildtype', # Link runtime libraries statically on Windows
    'optimization=2',
    'debug=false',
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

# parse compiler options
fc = meson.get_compiler('fortran')
fc_id = fc.get_id()
message('The fc_id is:', fc_id)
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
    compile_args += ['-fcheck=all', '-ffpe-trap=overflow,zero,invalid']
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
  
# Command line options for ifx
elif fc_id == 'intel-llvm-cl'
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

endif

# parallel build options
is_extended_build = get_option('extended')
is_cray = get_option('cray')
is_mpich = get_option('mpich')
if is_cray and build_machine.system() != 'linux'
  error('cray=true only supported on linux systems')
endif
if not is_extended_build
 is_extended_build = get_option('parallel')
endif
if is_cray and not is_extended_build
  is_extended_build = true
  is_mpich = true
endif
if is_mpich
  if is_cray
    mpifort_name = 'mpichf90' 
  else
    mpifort_name = 'mpichfort'
  endif
endif
message('Extended build:', is_extended_build)

# windows options for petsc
petsc_dir = meson.project_source_root() / '..' /'petsc-3.18.5'
petsc_arch = 'arch-ci-mswin-intel-modflow6'

# on windows only with intel
if build_machine.system() == 'windows' and is_extended_build
  if fc_id != 'intel-cl'
    error('Parallel build on Windows only with intel compiler. Terminating...')
  endif
endif

# lists for parallel dependencies and compiler arguments
dependencies = [ ]
extra_cmp_args = [ ]

# load petsc, mpi, and netcdf dependencies/libraries
if is_extended_build
  # find petsc
  if build_machine.system() != 'windows'
    petsc = dependency('PETSc', required : true)
  else
    # directly look for petsc
    petsc_compiled = petsc_dir / petsc_arch
    petsc = fc.find_library('libpetsc', dirs: petsc_compiled / 'lib', required : true)
  endif
  extra_cmp_args += [ '-D__WITH_PETSC__' ]
  dependencies += petsc
  with_petsc = true

  # find mpi
  if is_mpich
    mpifort = dependency(mpifort_name, required : true)
    dependencies += mpifort
  else
    mpi = dependency('mpi', language : 'fortran', required : true)
    dependencies += mpi
  endif
  extra_cmp_args += [ '-D__WITH_MPI__']
  with_mpi = true

  # find netcdf
  if build_machine.system() != 'windows'
    netcdf = dependency('netcdf', language : 'fortran', required : false)
  else
    # For CI testing only; Windows not yet supported
    nc_dir = meson.project_source_root() / '..' / 'ncf' / 'netcdf-fortran-4.6.1' / 'fortran'
    netcdf = fc.find_library('netcdff', dirs: [ nc_dir ], required : false, static : false)
    #nc_incdir = include_directories([ nc_dir ])
  endif
  if netcdf.found()
    with_netcdf = true
    extra_cmp_args += [ '-D__WITH_NETCDF__' ]
    dependencies += [ netcdf ]    
  endif
else
  with_netcdf = false
  with_petsc = false
  with_mpi = false
endif

compile_args += extra_cmp_args

add_project_arguments(fc.get_supported_arguments(compile_args), language: 'fortran')
add_project_link_arguments(fc.get_supported_arguments(link_args), language: 'fortran')

if is_extended_build and build_machine.system() == 'windows'
  message('Compiling PETSc Fortran modules')
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

# build mf6 and libmf6
buildname = get_option('buildname')
subdir('src')
subdir('srcbmi')

# build zbud6 and mf5to6 utility programs
subdir('utils')

# add autotest directory
fs = import('fs')
if fs.is_dir('autotest')
  subdir('autotest')
endif

# meson tests to evaluate installation success
testdir = meson.project_source_root() / '.mf6minsim'
if with_mpi
  mpiexec = find_program('mpiexec', required : false)
  if mpiexec.found()
    test('Parallel version command line test', mpiexec, args : ['-n', '2', mf6exe, '-v', '-p'])
    test('Parallel compiler command line test', mpiexec, args : ['-n', '2', mf6exe, '-c', '-p'])
    test('Serial simulation test', mf6exe, workdir : testdir)
    test('Parallel simulation test - 1 core', mpiexec, workdir : testdir, args : ['-n', '1', mf6exe, '-p'])
    test('Parallel simulation test - 2 cores', mpiexec, workdir : testdir, args : ['-n', '2', mf6exe, '-p'])
  endif
else
  test('Version command line test', mf6exe, args : ['-v',])
  test('Compiler command line test', mf6exe, args : ['-c',])
  test('Test installation help', mf6exe, args : ['-h',])
  test('Serial simulation test', mf6exe, workdir : testdir)
endif
