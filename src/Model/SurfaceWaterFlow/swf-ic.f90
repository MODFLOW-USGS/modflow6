! -- SWF Initial Conditions Module
! replicated from GwfIcModule to use IDM

module SwfIcModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LENMEMPATH
  use SimVariablesModule, only: errmsg
  use NumericalPackageModule, only: NumericalPackageType
  use BlockParserModule, only: BlockParserType
  use BaseDisModule, only: DisBaseType

  implicit none
  private
  public :: SwfIcType
  public :: ic_cr

  ! -- Most of the SwfIcType functionality is replicated from GWF
  type, extends(NumericalPackageType) :: SwfIcType
    real(DP), dimension(:), pointer, contiguous :: strt => null() ! starting stage
  contains
    procedure :: ic_ar
    procedure :: ic_da
    procedure :: ic_load
    procedure :: source_griddata
    procedure :: log_griddata
    procedure, private :: allocate_arrays
  end type SwfIcType

contains

  !> @brief create package
  !<
  subroutine ic_cr(ic, name_model, input_mempath, inunit, iout, dis)
    ! -- modules
    use MemoryManagerExtModule, only: mem_set_value
    ! -- dummy
    type(SwfIcType), pointer :: ic
    character(len=*), intent(in) :: name_model
    character(len=*), intent(in) :: input_mempath
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    class(DisBaseType), pointer, intent(in) :: dis
    ! -- local
    logical(LGP) :: found_fname
    ! -- formats
    character(len=*), parameter :: fmtheader = &
      "(1x, /1x, 'IC --  INITIAL CONDITIONS (IC) PACKAGE, VERSION 1, 10/10/2023', &
       &' INPUT READ FROM MEMPATH: ', A, /)"
    !
    ! -- Create the object
    allocate (ic)
    !
    ! -- create name and memory path
    call ic%set_names(1, name_model, 'IC', 'IC')
    !
    ! -- Allocate scalars
    call ic%allocate_scalars()
    !
    ! -- Set variables
    ic%input_mempath = input_mempath
    ic%inunit = inunit
    ic%iout = iout
    !
    ! -- set name of input file
    call mem_set_value(ic%input_fname, 'INPUT_FNAME', ic%input_mempath, &
                       found_fname)
    !
    ! -- set pointers
    ic%dis => dis
    !
    ! -- check if ic is enabled
    if (inunit > 0) then

      ! -- Print a message identifying the package.
      write (iout, fmtheader) input_mempath

      ! -- allocate arrays
      call ic%allocate_arrays()

      ! -- load ic
      call ic%ic_load()

    end if
    !
    ! -- Return
    return
  end subroutine ic_cr

  !> @brief allocate and read
  !!
  !! Set model dependent variable to initial conditions
  !!
  !<
  subroutine ic_ar(this, x)
    ! -- modules
    use SimModule, only: store_error
    ! -- dummy
    class(SwfIcType) :: this
    real(DP), dimension(:), intent(inout) :: x
    ! -- locals
    integer(I4B) :: n
    !
    ! -- Assign x equal to strt
    do n = 1, this%dis%nodes
      x(n) = this%strt(n)
    end do
    !
    ! -- Return
    return
  end subroutine ic_ar

  subroutine ic_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    use MemoryManagerExtModule, only: memorylist_remove
    use SimVariablesModule, only: idm_context
    ! -- dummy
    class(SwfIcType) :: this
    !
    ! -- Deallocate input memory
    call memorylist_remove(this%name_model, 'IC', idm_context)
    !
    ! -- deallocate parent
    call this%NumericalPackageType%da()
    !
    ! -- Scalars
    !
    ! -- Arrays
    call mem_deallocate(this%strt)
    !
    ! -- Return
    return
  end subroutine ic_da

  !> @brief load data from IDM to package
  !<
  subroutine ic_load(this)
    ! -- dummy
    class(SwfIcType) :: this
    ! -- locals
    !
    ! -- source input data
    call this%source_griddata()
    !
    ! -- Return
    return
  end subroutine ic_load

  !> @brief copy griddata from IDM to package
  !<
  subroutine source_griddata(this)
    ! -- modules
    use SimModule, only: count_errors, store_error
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerModule, only: mem_reallocate
    use MemoryManagerExtModule, only: mem_set_value
    use SimVariablesModule, only: idm_context
    use SwfIcInputModule, only: SwfIcParamFoundType
    ! -- dummy
    class(SwfIcType) :: this
    ! -- locals
    character(len=LENMEMPATH) :: idmMemoryPath
    type(SwfIcParamFoundType) :: found
    integer(I4B), dimension(:), pointer, contiguous :: map
    ! -- formats
    !
    ! -- set memory path
    idmMemoryPath = create_mem_path(this%name_model, 'IC', idm_context)
    !
    ! -- set map to convert user input data into reduced data
    map => null()
    if (this%dis%nodes < this%dis%nodesuser) map => this%dis%nodeuser
    !
    ! -- update defaults with idm sourced values
    call mem_set_value(this%strt, 'STRT', idmMemoryPath, map, found%strt)
    !
    ! -- ensure STRT was found
    if (.not. found%strt) then
      write (errmsg, '(a)') 'Error in GRIDDATA block: STRT not found.'
      call store_error(errmsg)
    end if
    !
    ! -- log griddata
    if (this%iout > 0) then
      call this%log_griddata(found)
    end if
    !
    ! -- Return
    return
  end subroutine source_griddata

  !> @brief log griddata to list file
  !<
  subroutine log_griddata(this, found)
    use SwfIcInputModule, only: SwfIcParamFoundType
    class(SwfIcType) :: this
    type(SwfIcParamFoundType), intent(in) :: found

    write (this%iout, '(1x,a)') 'Setting IC Griddata'

    if (found%strt) then
      write (this%iout, '(4x,a)') 'STRT set from input file'
    end if

    write (this%iout, '(1x,a,/)') 'End Setting IC Griddata'

  end subroutine log_griddata

  subroutine allocate_arrays(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(SwfIcType) :: this
    ! -- local
    !
    ! -- Allocate
    call mem_allocate(this%strt, this%dis%nodes, 'STRT', this%memoryPath)
    !
    ! -- Return
    return
  end subroutine allocate_arrays

end module SwfIcModule
