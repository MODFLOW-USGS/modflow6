!> @brief This module contains the IdmMf6FileLoaderModule
!!
!! This module contains the high-level routines for loading
!! a MODFLOW input file into the __INPUT__ memory manager
!! space.
!!
!<
module IdmMf6FileLoaderModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, LENMEMPATH, LENMODELNAME, &
                             LENPACKAGENAME, LENFTYPE, LENPACKAGETYPE
  use SimModule, only: store_error, store_error_filename
  use InputOutputModule, only: openfile, getunit
  use BlockParserModule, only: BlockParserType
  use ModflowInputModule, only: ModflowInputType, getModflowInput
  use CharacterStringModule, only: CharacterStringType
  use IdmPackageModule, only: ModelPackageInputsType

  implicit none
  private
  public :: input_load ! TODO: remove
  public :: load_models_mf6

  !> @brief derived type for storing package loader
  !!
  !! This derived type is used to store a pointer to a
  !! package load procedure.  This could be used to write
  !! a custom package loader as a way to override the
  !! generic_mf6_load routine.
  !!
  !<
  type :: PackageLoad
    procedure(IPackageLoad), nopass, pointer, public :: load_package => null() !< procedure pointer to the load routine
  end type PackageLoad

  abstract interface
    subroutine IPackageLoad(parser, mf6_input, iout)
      use KindModule, only: DP, I4B
      use BlockParserModule, only: BlockParserType
      use ModflowInputModule, only: ModflowInputType
      type(BlockParserType), intent(inout) :: parser !< block parser
      type(ModflowInputType), intent(in) :: mf6_input !< ModflowInputType object that describes the input
      integer(I4B), intent(in) :: iout !< unit number for output
    end subroutine IPackageLoad
  end interface

contains

  !> @brief generic procedure to MODFLOW 6 load routine
  !<
  subroutine generic_mf6_load(parser, mf6_input, iout)
    use LoadMf6FileTypeModule, only: idm_load
    type(BlockParserType), intent(inout) :: parser !< block parser
    type(ModflowInputType), intent(in) :: mf6_input !< ModflowInputType object that describes the input
    integer(I4B), intent(in) :: iout !< unit number for output

    call idm_load(parser, mf6_input%pkgtype, &
                  mf6_input%component_type, mf6_input%subcomponent_type, &
                  mf6_input%component_name, mf6_input%subcomponent_name, &
                  iout)

  end subroutine generic_mf6_load

  !> @brief input load for traditional mf6 simulation input file
  !<
  subroutine input_load(pkgtype, &
                        component_type, subcomponent_type, &
                        component_name, subcomponent_name, &
                        inunit, iout)
    character(len=*), intent(in) :: pkgtype !< pkgtype to load, such as DIS6, DISV6, NPF6
    character(len=*), intent(in) :: component_type !< component type, such as GWF or GWT
    character(len=*), intent(in) :: subcomponent_type !< subcomponent type, such as DIS or NPF
    character(len=*), intent(in) :: component_name !< component name, such as MYGWFMODEL
    character(len=*), intent(in) :: subcomponent_name !< subcomponent name, such as MYWELLPACKAGE
    integer(I4B), intent(in) :: inunit !< unit number for input
    integer(I4B), intent(in) :: iout !< unit number for output
    type(BlockParserType), allocatable :: parser !< block parser
    type(ModflowInputType) :: mf6_input
    type(PackageLoad) :: pkgloader
    !
    ! -- create description of input
    mf6_input = getModflowInput(pkgtype, component_type, &
                                subcomponent_type, component_name, &
                                subcomponent_name)
    !
    ! -- set mf6 parser based package loader by file type
    select case (pkgtype)
    case default
      allocate (parser)
      call parser%Initialize(inunit, iout)
      pkgloader%load_package => generic_mf6_load
    end select
    !
    ! -- invoke the selected load routine
    call pkgloader%load_package(parser, mf6_input, iout)
    !
    ! -- close files and deallocate
    if (allocated(parser)) then
      !call parser%clear()
      deallocate (parser)
    end if
    !
    ! -- return
    return
  end subroutine input_load

  !> @brief input load model idm supported package files
  !<
  subroutine load_model_pkgfiles(model_pkg_inputs, iout)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    use MemoryHelperModule, only: create_mem_path
    use SimVariablesModule, only: idm_context
    use IdmDfnSelectorModule, only: idm_integrated, idm_multi_package
    ! -- dummy
    type(ModelPackageInputsType), intent(inout) :: model_pkg_inputs
    integer(I4B), intent(in) :: iout
    ! -- locals
    integer(I4B) :: n, m
    character(len=LENMEMPATH) :: mempath
    character(len=LENPACKAGETYPE) :: pkgtype
    character(len=LENPACKAGENAME) :: sc_name
    character(len=LINELENGTH), pointer :: cstr
    !
    do n = 1, size(model_pkg_inputs%pkglist)
      !
      ! -- this list package type
      pkgtype = model_pkg_inputs%pkglist(n)%pkgtype
      !
      ! -- open each package type instance file
      do m = 1, model_pkg_inputs%pkglist(n)%pnum
        !
        ! -- load package to input context if using IDM
        if (idm_integrated(model_pkg_inputs%component_type, &
                           model_pkg_inputs%pkglist(n)%component_type)) then
          !
          ! -- set subcomponent name
          if (idm_multi_package(model_pkg_inputs%component_type, &
                                model_pkg_inputs%pkglist(n)%component_type)) then
            !
            sc_name = model_pkg_inputs%pkglist(n)%pkgnames(m)
          else
            !
            sc_name = model_pkg_inputs%pkglist(n)%component_type
          end if
          !
          ! -- load model package to input context
          call input_load(pkgtype, model_pkg_inputs%component_type, &
                          model_pkg_inputs%pkglist(n)%component_type, &
                          model_pkg_inputs%modelname, sc_name, &
                          model_pkg_inputs%pkglist(n)%inunits(m), iout)
          !
          ! -- close file and update unit number
          close (model_pkg_inputs%pkglist(n)%inunits(m))
          model_pkg_inputs%pkglist(n)%inunits(m) = 0
          !
          ! -- set package mempath for filename
          mempath = create_mem_path(model_pkg_inputs%modelname, sc_name, idm_context)
          !
          ! -- allocate and set input filename for package
          call mem_allocate(cstr, LINELENGTH, 'INPUT_FNAME', mempath)
          cstr = model_pkg_inputs%pkglist(n)%filenames(m)
          !
        else
          ! Not an IDM supported package, leave inunit open
        end if
      end do
    end do
    !
    ! -- return
    return
  end subroutine load_model_pkgfiles

  !> @brief open all model package files
  !<
  subroutine open_model_pkgfiles(model_pkg_inputs, iout)
    ! -- modules
    ! -- dummy
    type(ModelPackageInputsType), intent(inout) :: model_pkg_inputs
    integer(I4B), intent(in) :: iout
    ! -- locals
    integer(I4B) :: n, m
    character(len=20) :: accarg, fmtarg, filstat
    character(len=LINELENGTH) :: filename
    character(len=LENPACKAGETYPE) :: filetype
    character(len=LINELENGTH) :: errmsg
    !
    ! -- initialize read attributes
    accarg = 'SEQUENTIAL'
    fmtarg = 'FORMATTED'
    filstat = 'OLD'
    !
    do n = 1, size(model_pkg_inputs%pkglist)
      !
      ! -- this list package type
      filetype = model_pkg_inputs%pkglist(n)%pkgtype
      !
      ! -- open each package type instance file
      do m = 1, model_pkg_inputs%pkglist(n)%pnum
        !
        ! -- set filename
        filename = model_pkg_inputs%pkglist(n)%filenames(m)
        !
        if (filename /= '') then
          !
          ! -- get unit number and open file
          model_pkg_inputs%pkglist(n)%inunits(m) = getunit()
          call openfile(model_pkg_inputs%pkglist(n)%inunits(m), iout, &
                        trim(adjustl(filename)), filetype, fmtarg, accarg, &
                        filstat)
        !
        else
          write (errmsg, '(a,a,a,a,a)') &
            'Package file unspecified, cannot load model package &
            &[model=', trim(model_pkg_inputs%modelname), &
            ', type=', trim(filetype), '].'
          call store_error(errmsg)
          call store_error_filename(model_pkg_inputs%modelfname)
        end if
      end do
    end do
    !
    ! -- returh
    return
  end subroutine open_model_pkgfiles

  !> @brief load and make pkg info available to models
  !<
  subroutine modelpkgs_load(mtype, mfname, mname, niunit, cunit, iout)
    ! -- modules
    ! -- dummy
    character(len=*), intent(in) :: mtype
    character(len=*), intent(in) :: mfname
    character(len=*), intent(in) :: mname
    integer(I4B), intent(in) :: niunit
    character(len=*), dimension(niunit), intent(in) :: cunit
    integer(I4B), intent(in) :: iout
    ! -- locals
    type(ModelPackageInputsType) :: model_pkg_inputs
    !
    ! -- initialize model package object
    call model_pkg_inputs%init(mtype, mfname, mname, niunit, cunit, iout)
    !
    ! -- open model package files
    call open_model_pkgfiles(model_pkg_inputs, iout)
    !
    ! -- load model idm integrated package files
    call load_model_pkgfiles(model_pkg_inputs, iout)
    !
    ! -- load descriptions of packages to model input context
    call model_pkg_inputs%memload()
    !
    ! -- cleanup
    call model_pkg_inputs%destroy()
    !
    ! -- return
    return
  end subroutine modelpkgs_load

  !> @brief input load a single model namfile and model package files
  !<
  subroutine model_load(mtype, mfname, mname, iout)
    ! -- modules
    use SimVariablesModule, only: simfile
    use IdmPackageModule, only: supported_model_packages
    ! -- dummy
    character(len=*), intent(in) :: mtype
    character(len=*), intent(in) :: mfname
    character(len=*), intent(in) :: mname
    integer(I4B), intent(in) :: iout
    ! -- locals
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: inunit
    character(len=LENPACKAGETYPE), dimension(:), allocatable :: pkgtypes
    integer(I4B) :: numpkgs
    !
    ! -- open namfile
    inunit = getunit()
    call openfile(inunit, iout, trim(mfname), 'NAM')
    !
    ! -- allocate and set model supported package types
    call supported_model_packages(mtype, pkgtypes, numpkgs)
    !
    select case (mtype)
    case ('GWF6')
      !
      ! -- load model namfile to the input context
      call input_load('GWF6', 'GWF', 'NAM', mname, 'NAM', inunit, iout)
      !
      ! -- load and create descriptions of model package files
      call modelpkgs_load(mtype, mfname, mname, numpkgs, pkgtypes, iout)
      !
    case ('GWT6')
      !
      call input_load('GWT6', 'GWT', 'NAM', mname, 'NAM', inunit, iout)
      !
      call modelpkgs_load(mtype, mfname, mname, numpkgs, pkgtypes, iout)
      !
    case default
      write (errmsg, '(a,a,a,a,a)') &
        'Unknown simulation model type &
        &[model=', trim(mname), &
        ', type=', trim(mtype), '].'
      call store_error(errmsg)
      call store_error_filename(simfile)
    end select
    !
    ! -- close namfile
    close (inunit)
    !
    ! -- cleanup
    if (allocated(pkgtypes)) deallocate (pkgtypes)
    !
    ! -- return
    return
  end subroutine model_load

  !> @brief input load model namfiles and model package files
  !<
  subroutine load_models_mf6(model_loadmask, iout)
    ! -- modules
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerModule, only: mem_setptr
    use CharacterStringModule, only: CharacterStringType
    use SimVariablesModule, only: idm_context
    ! -- dummy
    integer(I4B), dimension(:), intent(in) :: model_loadmask
    integer(I4B), intent(in) :: iout
    ! -- locals
    character(len=LENMEMPATH) :: input_mempath
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: mtypes !< model types
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: mfnames !< model file names
    type(CharacterStringType), dimension(:), contiguous, &
      pointer :: mnames !< model names
    character(len=LINELENGTH) :: mtype, mfname
    character(len=LENMODELNAME) :: mname
    integer(I4B) :: n
    !
    ! -- set input memory path
    input_mempath = create_mem_path('SIM', 'NAM', idm_context)
    !
    ! -- set pointers to input context model attribute arrays
    call mem_setptr(mtypes, 'MTYPE', input_mempath)
    call mem_setptr(mfnames, 'MFNAME', input_mempath)
    call mem_setptr(mnames, 'MNAME', input_mempath)
    !
    do n = 1, size(mtypes)
      !
      ! -- attributes for this model
      mtype = mtypes(n)
      mfname = mfnames(n)
      mname = mnames(n)
      !
      ! -- load model namfile
      if (model_loadmask(n) > 0) then
        call model_load(mtype, mfname, mname, iout)
      end if
    end do
    !
    ! -- return
    return
  end subroutine load_models_mf6

end module IdmMf6FileLoaderModule
