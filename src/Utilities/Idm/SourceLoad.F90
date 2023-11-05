!> @brief This module contains the SourceLoadModule
!!
!! This module contains the routines needed to generate
!! a loader object for an input source and routines
!! that distribute processing to a particular source.
!!
!<
module SourceLoadModule

  use KindModule, only: DP, I4B, LGP
  use SimVariablesModule, only: errmsg
  use ConstantsModule, only: LINELENGTH, LENMEMPATH, LENMODELNAME, LENFTYPE, &
                             LENPACKAGETYPE, LENPACKAGENAME
  use SimModule, only: store_error, store_error_filename
  use ModflowInputModule, only: ModflowInputType, getModflowInput

  implicit none
  private
  public :: create_pkg_loader
  public :: open_source_file
  public :: load_modelnam, load_simnam
  public :: create_context

contains

  !> @brief factory function to create and setup model package static loader
  !<
  function create_pkg_loader(component_type, subcomponent_type, pkgname, &
                             pkgtype, filename, modelname, modelfname) &
    result(loader)
    use SourceCommonModule, only: package_source_type, subcomponent_name
    use InputLoadTypeModule, only: StaticPkgLoadBaseType
    character(len=*), intent(in) :: component_type
    character(len=*), intent(in) :: subcomponent_type
    character(len=*), intent(in) :: pkgname
    character(len=*), intent(in) :: pkgtype
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: modelfname
    class(StaticPkgLoadBaseType), pointer :: loader
    type(ModflowInputType) :: mf6_input
    character(len=LENPACKAGENAME) :: source_type
    character(len=LENPACKAGENAME) :: sc_name
    !
    ! -- set subcomponent name
    sc_name = subcomponent_name(component_type, subcomponent_type, pkgname)
    !
    ! -- create description of input
    mf6_input = getModflowInput(pkgtype, component_type, subcomponent_type, &
                                modelname, sc_name, filename)
    !
    ! -- set package source
    source_type = package_source_type(filename)
    !
    ! -- set source loader for model package
    loader => package_loader(source_type, modelfname)
    !
    ! -- initialize loader
    call loader%init(mf6_input, modelname, modelfname, filename)
    !
    ! -- return
    return
  end function create_pkg_loader

  !> @brief allocate source model package static loader
  !<
  function package_loader(source_type, modelfname) result(loader)
    use InputLoadTypeModule, only: StaticPkgLoadBaseType
    use IdmMf6FileModule, only: Mf6FileStaticPkgLoadType
#if defined(__WITH_NETCDF__)
    use IdmNetCDFFileModule, only: NC4StaticPkgLoadType
#endif
    character(len=*), intent(inout) :: source_type
    character(len=*), intent(in) :: modelfname
    class(Mf6FileStaticPkgLoadType), pointer :: mf6file_loader
#if defined(__WITH_NETCDF__)
    class(NC4StaticPkgLoadType), pointer :: nc4_loader
#endif
    class(StaticPkgLoadBaseType), pointer :: loader
    !
    ! -- initialize
    nullify (loader)
    !
    ! -- allocate derived object
    select case (source_type)
    case ('MF6FILE')
      allocate (mf6file_loader)
      loader => mf6file_loader
    case ('NETCDF4')
#if defined(__WITH_NETCDF__)
      allocate (nc4_loader)
      loader => nc4_loader
#else
      write (errmsg, '(a)') &
        'Cannot load package inputs. NetCDF4 input file provided &
        &but NetCDF4 libraries not available.'
      call store_error(errmsg)
      call store_error_filename(modelfname)
#endif
    case default
      write (errmsg, '(a)') &
        'Simulation package input source type "'//trim(source_type)// &
        '" not currently supported.'
      call store_error(errmsg)
      call store_error_filename(modelfname)
    end select
    !
    ! -- return
    return
  end function package_loader

  function open_source_file(pkgtype, filename, modelfname, iout) result(fd)
    use SourceCommonModule, only: package_source_type
    use IdmMf6FileModule, only: open_mf6file
    character(len=*), intent(in) :: pkgtype
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: modelfname
    integer(I4B), intent(in) :: iout
    integer(I4B) :: fd
    character(len=LENPACKAGENAME) :: source_type
    !
    ! -- initialize
    fd = 0
    !
    ! -- set source type
    source_type = package_source_type(filename)
    !
    select case (source_type)
    case ('MF6FILE')
      fd = open_mf6file(pkgtype, filename, modelfname, iout)
    case ('NETCDF4')
#if defined(__WITH_NETCDF__)
      ! -- no-op, don't provide ncid fd to a package
#else
      write (errmsg, '(a)') &
        'Cannot open source input file. NetCDF4 input file provided &
        &but NetCDF4 libraries not available.'
      call store_error(errmsg)
      call store_error_filename(modelfname)
#endif
    case default
    end select
    !
    ! -- return
    return
  end function open_source_file

  subroutine load_modelnam(mtype, mfname, mname, iout)
    use SimVariablesModule, only: simfile
    use SourceCommonModule, only: package_source_type, idm_component_type
    use IdmMf6FileModule, only: input_load
    character(len=*), intent(in) :: mtype
    character(len=*), intent(in) :: mfname
    character(len=*), intent(in) :: mname
    integer(I4B), intent(in) :: iout
    type(ModflowInputType) :: mf6_input
    character(len=LENPACKAGENAME) :: source_type
    !
    ! -- set source type
    source_type = package_source_type(mfname)
    !
    ! -- create description of input
    mf6_input = getModflowInput(mtype, idm_component_type(mtype), &
                                'NAM', mname, 'NAM')
    !
    select case (source_type)
    case ('MF6FILE')
      call input_load(mfname, mf6_input, simfile, iout)
    case ('NETCDF4')
#if defined(__WITH_NETCDF__)
      write (errmsg, '(a)') &
        'NetCDF4 Model name files not currently supported.'
      call store_error(errmsg)
      call store_error_filename(simfile)
#else
      write (errmsg, '(a)') &
        'Cannot load name file. NetCDF4 input file provided &
        &but but NetCDF4 libraries not available.'
      call store_error(errmsg)
      call store_error_filename(simfile)
#endif
    case default
    end select
    !
    ! -- return
    return
  end subroutine load_modelnam

  subroutine load_simnam()
    use SimVariablesModule, only: simfile, iout
    use GenericUtilitiesModule, only: sim_message
    use IdmMf6FileModule, only: input_load
    type(ModflowInputType) :: mf6_input
    character(len=LINELENGTH) :: line
    logical :: lexist
    !
    ! -- load mfsim.nam if it exists
    inquire (file=trim(adjustl(simfile)), exist=lexist)
    !
    if (lexist) then
      !
      ! -- write name of namfile to stdout
      write (line, '(2(1x,a))') 'Using Simulation name file:', &
        trim(adjustl(simfile))
      call sim_message(line, skipafter=1)
      !
      ! -- create description of input
      mf6_input = getModflowInput('NAM6', 'SIM', 'NAM', 'SIM', 'NAM')
      !
      ! -- open namfile and load to input context
      call input_load(simfile, mf6_input, simfile, iout)
    end if
    !
    ! -- return
    return
  end subroutine load_simnam

  !> @brief create model context
  !<
  subroutine create_context(modeltype, component_type, modelname, &
                            modelfname, pkglist, iout)
    ! -- modules
    use ModelPackageInputsModule, only: LoadablePackageType
    ! -- drummy
    character(len=*), intent(in) :: modeltype
    character(len=*), intent(in) :: component_type
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: modelfname
    type(LoadablePackageType), dimension(:), &
      allocatable, intent(in) :: pkglist
    integer(I4B), intent(in) :: iout
    ! -- local
    !
    ! -- create netcdf4 context
    call create_nc4_context(modeltype, component_type, modelname, &
                            modelfname, pkglist, iout)
    !
    ! -- return
    return
  end subroutine create_context

  !> @brief create model netcdf4 context
  !<
  subroutine create_nc4_context(modeltype, component_type, modelname, &
                                modelfname, pkglist, iout)
    use ModelPackageInputsModule, only: LoadablePackageType
    use NC4ModelInputsModule, only: NC4ModelInputsType
    use InputModelContextModule, only: AddModelNC4Context
#if defined(__WITH_NETCDF__)
    use LoadNetCDFDataModule, only: nc4_pkg_context
    use IdmNetCDFFileModule, only: open_ncfile
#endif
    ! -- drummy
    character(len=*), intent(in) :: modeltype
    character(len=*), intent(in) :: component_type
    character(len=*), intent(in) :: modelname
    character(len=*), intent(in) :: modelfname
    type(LoadablePackageType), dimension(:), &
      allocatable, intent(in) :: pkglist
    integer(I4B), intent(in) :: iout
    ! -- local
    character(len=LINELENGTH) :: nc4_fname
    type(NC4ModelInputsType), pointer :: nc4_context
#if defined(__WITH_NETCDF__)
    integer(I4B) :: ncid
#endif
    !
    ! -- allocate context object
    allocate (nc4_context)
    !
    ! -- set model nc filename if provided
    nc4_fname = nc4_filename(pkglist, modelfname)
    !
    if (nc4_fname /= '') then
#if defined(__WITH_NETCDF__)
      !
      ! -- open nc4 input file
      ncid = open_ncfile(nc4_fname, iout)
      !
      ! -- init model context object
      call nc4_context%init(modeltype, component_type, modelname, nc4_fname, ncid)
      !
      ! -- add NETCDF4 packages to context
      call set_nc4_pkglist(component_type, modelfname, pkglist, &
                           nc4_context, nc4_fname)
      !
      ! -- read the file and build the context
      call nc4_pkg_context(nc4_context, iout)
      !
#else
      write (errmsg, '(a)') &
        'Cannot load model packages. NetCDF4 input file &
        &specified in model namefile options block but &
        &NetCDF4 libraries are not available.'
      call store_error(errmsg)
      call store_error_filename(modelfname)
#endif
    else
      !
      ! -- initialize object
      call nc4_context%init(modeltype, component_type, modelname, '', 0)
      !
    end if
    !
    ! -- add context to model context list
    call AddModelNC4Context(modelname, modelfname, nc4_context)
    !
    ! -- return
    return
  end subroutine create_nc4_context

#if defined(__WITH_NETCDF__)
  subroutine set_nc4_pkglist(component_type, modelfname, pkglist, &
                             nc4_context, nc4_fname)
    use SourceCommonModule, only: package_source_type
    use IdmDfnSelectorModule, only: idm_integrated
    use ModelPackageInputsModule, only: LoadablePackageType
    use NC4ModelInputsModule, only: NC4ModelInputsType
    ! -- drummy
    character(len=*), intent(in) :: component_type
    character(len=*), intent(in) :: modelfname
    type(LoadablePackageType), dimension(:), &
      allocatable, intent(in) :: pkglist
    type(NC4ModelInputsType), pointer, &
      intent(inout) :: nc4_context
    character(len=*) :: nc4_fname
    ! -- local
    integer(I4B) :: m, n
    !
    ! -- add NETCDF4 packages to context list
    do n = 1, size(pkglist)
      ! -- load package instances
      do m = 1, pkglist(n)%pnum
        !
        if (package_source_type(pkglist(n)%filenames(m)) == 'NETCDF4') then
          !
          if (idm_integrated(component_type, pkglist(n)%subcomponent_type)) then
            !
            ! -- add pkg instance to context object
            call nc4_context%add(pkglist(n)%pkgtype, &
                                 pkglist(n)%pkgnames(m), &
                                 pkglist(n)%subcomponent_type)
            !
          else
            errmsg = 'NetCDF4 is unsupported for package type "'// &
                     trim(pkglist(n)%pkgtype)//'".'
            call store_error(errmsg)
            call store_error_filename(modelfname)
          end if
        end if
      end do
    end do
    !
    ! -- return
    return
  end subroutine set_nc4_pkglist
#endif

  !> @brief set model netcdf4 input filename
  !<
  function nc4_filename(pkglist, modelfname) result(nc4_fname)
    use ModelPackageInputsModule, only: LoadablePackageType
    use SourceCommonModule, only: package_source_type
    ! -- drummy
    type(LoadablePackageType), dimension(:), &
      allocatable, intent(in) :: pkglist
    character(len=*), intent(in) :: modelfname
    ! -- local
    character(len=LINELENGTH) :: nc4_fname
    integer(I4B) :: m, n
    !
    ! -- initialize
    nc4_fname = ''
    !
    ! -- verify single model nc file
    do n = 1, size(pkglist)
      do m = 1, pkglist(n)%pnum
        if (package_source_type(pkglist(n)%filenames(m)) == 'NETCDF4') then
          if (nc4_fname == '') then
            nc4_fname = pkglist(n)%filenames(m)
          else if (nc4_fname /= pkglist(n)%filenames(m)) then
            nc4_fname = ''
            errmsg = 'Multiple *.nc model input files detected in packages &
                     &block. Only one model NetCDF4 input supported.'
            call store_error(errmsg)
            call store_error_filename(modelfname)
          end if
        end if
      end do
    end do
    !
    ! -- return
    return
  end function nc4_filename

end module SourceLoadModule
