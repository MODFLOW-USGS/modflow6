!> @brief This module contains the IdmMf6FileLoaderModule
!!
!! This module contains the high-level routines for loading
!! a MODFLOW input file into the __INPUT__ memory manager
!! space.
!!
!<
module IdmMf6FileLoaderModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH, LENMEMPATH, LENMODELNAME
  use SimModule, only: store_error
  use InputOutputModule, only: openfile, getunit
  use BlockParserModule, only: BlockParserType
  use ModflowInputModule, only: ModflowInputType, getModflowInput

  implicit none
  private
  public :: input_load
  public :: load_model_inputs

  interface input_load
    module procedure input_load_blockparser, input_load_generic
  end interface input_load

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

    call idm_load(parser, mf6_input%file_type, &
                  mf6_input%component_type, mf6_input%subcomponent_type, &
                  mf6_input%component_name, mf6_input%subcomponent_name, &
                  iout)

  end subroutine generic_mf6_load

  !> @brief main entry to mf6 input load
  !<
  subroutine input_load_blockparser(parser, filetype, &
                                    component_type, subcomponent_type, &
                                    component_name, subcomponent_name, &
                                    iout)
    type(BlockParserType), intent(inout) :: parser !< block parser
    character(len=*), intent(in) :: filetype !< file type to load, such as DIS6, DISV6, NPF6
    character(len=*), intent(in) :: component_type !< component type, such as GWF or GWT
    character(len=*), intent(in) :: subcomponent_type !< subcomponent type, such as DIS or NPF
    character(len=*), intent(in) :: component_name !< component name, such as MYGWFMODEL
    character(len=*), intent(in) :: subcomponent_name !< subcomponent name, such as MYWELLPACKAGE
    integer(I4B), intent(in) :: iout !< unit number for output
    type(ModflowInputType) :: mf6_input
    type(PackageLoad) :: pkgloader

    mf6_input = getModflowInput(filetype, component_type, &
                                subcomponent_type, component_name, &
                                subcomponent_name)
    !
    ! -- set mf6 parser based package loader by file type
    select case (filetype)
    case default
      pkgloader%load_package => generic_mf6_load
    end select
    !
    ! -- invoke the selected load routine
    call pkgloader%load_package(parser, mf6_input, iout)
    !
    ! -- return
    return
  end subroutine input_load_blockparser

  !> @brief main entry to mf6 input load
  !<
  subroutine input_load_generic(filetype, &
                                component_type, subcomponent_type, &
                                component_name, subcomponent_name, &
                                inunit, iout)
    character(len=*), intent(in) :: filetype !< file type to load, such as DIS6, DISV6, NPF6
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
    mf6_input = getModflowInput(filetype, component_type, &
                                subcomponent_type, component_name, &
                                subcomponent_name)
    !
    ! -- set mf6 parser based package loader by file type
    select case (filetype)
    case default
      allocate (parser)
      call parser%Initialize(inunit, iout)
      pkgloader%load_package => generic_mf6_load
    end select
    !
    ! -- invoke the selected load routine
    call pkgloader%load_package(parser, mf6_input, iout)
    !
    ! -- deallocate
    if (allocated(parser)) deallocate (parser)
    !
    ! -- return
    return
  end subroutine input_load_generic

  !> @brief load files listed in model namfile packages block
  !<
  subroutine load_package_inputs(mtype, mfname, mname, iout)
    ! -- modules
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerModule, only: mem_setptr
    use CharacterStringModule, only: CharacterStringType
    use SimVariablesModule, only: idm_context
    ! -- dummy
    character(len=*), intent(in) :: mtype
    character(len=*), intent(in) :: mfname
    character(len=*), intent(in) :: mname
    integer(I4B), intent(in) :: iout
    ! -- locals
    character(len=LENMEMPATH) :: idmMemoryPath
    type(CharacterStringType), dimension(:), pointer, contiguous :: ftypes !< file types
    type(CharacterStringType), dimension(:), pointer, contiguous :: fnames !< file names
    type(CharacterStringType), dimension(:), pointer, contiguous :: pnames !< package names
    character(len=LINELENGTH) :: ftype, fname, pname
    integer(I4B) :: n, idis, idsp, inpf
    character(len=LINELENGTH) :: errmsg
    logical(LGP) :: terminate = .true.
    !
    ! -- set input memory path
    idmMemoryPath = create_mem_path(mname, 'NAM', idm_context)
    !
    ! -- initialize indexes
    idis = 0
    idsp = 0
    inpf = 0
    !
    ! -- set pointers to input context package attribute arrays
    call mem_setptr(ftypes, 'FTYPE', idmMemoryPath)
    call mem_setptr(fnames, 'FNAME', idmMemoryPath)
    call mem_setptr(pnames, 'PNAME', idmMemoryPath)
    !
    ! -- model packages
    ! -- dis must be loaded before other packages
    do n = 1, size(ftypes)
      !
      ! -- attributes for this package
      ftype = ftypes(n)
      fname = fnames(n)
      pname = pnames(n)
      !
      ! -- load dis, store indexes
      select case (ftype)
      case ('DIS6')
        call check_package_duplicates(mname, ftype, idis, n)
        call load_package(fname, 'DIS6', 'GWF', 'DIS', mname, 'DIS', iout)
      case ('DISU6')
        call check_package_duplicates(mname, ftype, idis, n)
        call load_package(fname, 'DISU6', 'GWF', 'DISU', mname, 'DISU', iout)
      case ('DISV6')
        call check_package_duplicates(mname, ftype, idis, n)
        call load_package(fname, 'DISV6', 'GWF', 'DISV', mname, 'DISV', iout)
      case ('DSP6')
        call check_package_duplicates(mname, ftype, idsp, n)
      case ('NPF6')
        call check_package_duplicates(mname, ftype, inpf, n)
      case default
      end select
    end do
    !
    ! -- Verify discretization is loaded
    if (idis == 0) then
      write (errmsg, '(1x,a,a,a)') &
        '****ERROR[', trim(mname), &
        ']. Discretization (DIS6, DISV6, or DISU6) Package not specified.'
      call store_error(errmsg, terminate)
    end if
    !
    ! -- load DSP package inputs
    if (idsp > 0) then
      fname = fnames(idsp)
      call load_package(fname, 'DSP6', 'GWT', 'DSP', mname, 'DSP', iout)
    end if
    !
    ! -- load NPF package inputs
    if (inpf > 0) then
      fname = fnames(inpf)
      call load_package(fname, 'NPF6', 'GWF', 'NPF', mname, 'NPF', iout)
    end if
    !
    ! -- return
    return
  end subroutine load_package_inputs

  !> @brief load a model package file
  !<
  subroutine load_package(fname, ftype, ctype, sctype, cname, scname, iout)
    ! -- modules
    ! -- dummy
    character(len=*), intent(in) :: fname !< file name
    character(len=*), intent(in) :: ftype !< file type
    character(len=*), intent(in) :: ctype !< component type
    character(len=*), intent(in) :: sctype !< subcomponent type
    character(len=*), intent(in) :: cname !< component name
    character(len=*), intent(in) :: scname ! subcomponent name
    integer(I4B), intent(in) :: iout
    ! -- locals
    integer(I4B) :: inunit
    character(len=20) :: accarg, fmtarg, filstat
    !
    ! -- initialize read attributes
    accarg = 'SEQUENTIAL'
    fmtarg = 'FORMATTED'
    filstat = 'OLD'
    !
    ! -- open file
    inunit = getunit()
    call openfile(inunit, iout, trim(adjustl(fname)), &
                  ftype, fmtarg, accarg, filstat)
    !
    ! -- load to input context
    call input_load(ftype, ctype, sctype, cname, scname, inunit, iout)
    !
    ! -- close file
    close (inunit)
    !
    ! -- return
    return
  end subroutine load_package

  !> @brief verify package is unique for model
  !<
  subroutine check_package_duplicates(mname, ftype, current_index, &
                                      candidate_index)
    ! -- modules
    ! -- dummy
    character(len=*), intent(in) :: mname
    character(len=*), intent(in) :: ftype
    integer(I4B), intent(inout) :: current_index
    integer(I4B), intent(in) :: candidate_index
    ! -- locals
    character(len=LINELENGTH) :: errmsg
    logical(LGP) :: terminate = .true.
    !
    if (current_index > 0) then
      write (errmsg, '(1x,a,a,a,a,a)') &
        '****ERROR[', trim(mname), '][', trim(ftype), &
        ']. Duplicate package instances specified in model namfile for file type.'
      call store_error(errmsg, terminate)
    else
      current_index = candidate_index
    end if
    !
    ! -- return
    return
  end subroutine check_package_duplicates

  !> @brief load a model namfile
  !<
  subroutine model_load(mtype, mfname, mname, iout)
    ! -- modules
    ! -- dummy
    character(len=*), intent(in) :: mtype
    character(len=*), intent(in) :: mfname
    character(len=*), intent(in) :: mname
    integer(I4B), intent(in) :: iout
    ! -- locals
    character(len=LINELENGTH) :: errmsg
    integer(I4B) :: inunit
    logical(LGP) :: terminate = .true.
    !
    ! -- open namfile
    inunit = getunit()
    call openfile(inunit, iout, trim(mfname), 'NAM')
    !
    ! -- load model input
    select case (mtype)
    case ('GWF6')
      call input_load('GWF6', 'GWF', 'NAM', mname, 'NAM', inunit, iout)
      call load_package_inputs(mtype, mfname, mname, iout)
    case ('GWT6')
      call input_load('GWT6', 'GWT', 'NAM', mname, 'NAM', inunit, iout)
      call load_package_inputs(mtype, mfname, mname, iout)
    case default
      write (errmsg, '(1x,a,a,a,a)') &
        '****ERROR[', trim(mname), &
        ']. Unknown Simulation Modeltype: ', trim(mtype)
      call store_error(errmsg, terminate)
    end select
    !
    ! -- close file
    close (inunit)
    !
    ! -- return
    return
  end subroutine model_load

  !> @brief load model namfiles
  !<
  subroutine load_model_inputs(iout)
    ! -- modules
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerModule, only: mem_setptr
    use CharacterStringModule, only: CharacterStringType
    use SimVariablesModule, only: idm_context
    ! -- dummy
    integer(I4B), intent(in) :: iout
    ! -- locals
    character(len=LENMEMPATH) :: idmMemoryPath
    type(CharacterStringType), dimension(:), pointer, contiguous :: mtypes !< model types
    type(CharacterStringType), dimension(:), pointer, contiguous :: mfnames !< model file names
    type(CharacterStringType), dimension(:), pointer, contiguous :: mnames !< model names
    character(len=LINELENGTH) :: mtype, mfname
    character(len=LENMODELNAME) :: mname
    integer(I4B) :: n
    !
    ! -- set input memory path
    idmMemoryPath = create_mem_path('SIM', 'NAM', idm_context)
    !
    ! -- set pointers to input context model attribute arrays
    call mem_setptr(mtypes, 'MTYPE', idmMemoryPath)
    call mem_setptr(mfnames, 'MFNAME', idmMemoryPath)
    call mem_setptr(mnames, 'MNAME', idmMemoryPath)
    !
    do n = 1, size(mtypes)
      !
      ! -- attributes for this model
      mtype = mtypes(n)
      mfname = mfnames(n)
      mname = mnames(n)
      !
      ! -- load model namfile
      call model_load(mtype, mfname, mname, iout)
    end do
    !
    ! -- return
    return
  end subroutine load_model_inputs

end module IdmMf6FileLoaderModule
