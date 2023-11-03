module GwfIcModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: LINELENGTH
  use NumericalPackageModule, only: NumericalPackageType
  use BlockParserModule, only: BlockParserType
  use BaseDisModule, only: DisBaseType

  implicit none
  private
  public :: GwfIcType
  public :: ic_cr

  type, extends(NumericalPackageType) :: GwfIcType
    real(DP), dimension(:), pointer, contiguous :: strt => null() ! starting head
  contains
    procedure :: ic_load
    procedure :: ic_da
    procedure, private :: allocate_arrays
    procedure, private :: source_griddata
  end type GwfIcType

contains

  !> @brief Create a new initial conditions object
  subroutine ic_cr(ic, name_model, input_mempath, inunit, iout, dis)
    ! -- modules
    use MemoryManagerExtModule, only: mem_set_value
    ! -- dummy
    type(GwfIcType), pointer :: ic
    character(len=*), intent(in) :: name_model
    character(len=*), intent(in) :: input_mempath
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    class(DisBaseType), pointer, intent(in) :: dis
    ! -- formats
    character(len=*), parameter :: fmtic = &
      "(1x, /1x, 'IC -- Initial Conditions Package, Version 8, 3/28/2015', &
      &' input read from mempath: ', A, //)"
    !
    ! -- create IC object
    allocate (ic)
    !
    ! -- create name and memory path
    call ic%set_names(1, name_model, 'IC', 'IC', input_mempath)
    !
    ! -- allocate scalars
    call ic%allocate_scalars()
    !
    ! -- set variables
    ic%inunit = inunit
    ic%iout = iout
    !
    ! -- set points
    ic%dis => dis
    !
    ! -- if package is enabled, print message identifying it
    if (inunit > 0) &
      write (ic%iout, fmtic) input_mempath
  end subroutine ic_cr

  !> @brief Load initial conditions
  subroutine ic_load(this, x)
    ! -- modules
    use BaseDisModule, only: DisBaseType
    use SimModule, only: store_error
    ! -- dummy
    class(GwfIcType) :: this
    real(DP), dimension(:), intent(inout) :: x
    ! -- locals
    integer(I4B) :: n
    !
    ! -- allocate arrays
    call this%allocate_arrays(this%dis%nodes)
    !
    ! -- read grid data
    call this%source_griddata()
    !
    ! -- assign starting head
    do n = 1, this%dis%nodes
      x(n) = this%strt(n)
    end do
  end subroutine ic_load

  !> @brief Deallocate
  subroutine ic_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    use MemoryManagerExtModule, only: memorylist_remove
    use SimVariablesModule, only: idm_context
    ! -- dummy
    class(GwfIcType) :: this
    !
    ! -- deallocate IDM memory
    call memorylist_remove(this%name_model, 'IC', idm_context)
    !
    ! -- deallocate arrays
    call mem_deallocate(this%strt)
    !
    ! -- deallocate parent
    call this%NumericalPackageType%da()
  end subroutine ic_da

  ! @brief Allocate arrays
  subroutine allocate_arrays(this, nodes)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwfIcType) :: this
    integer(I4B), intent(in) :: nodes
    !
    ! -- Allocate
    call mem_allocate(this%strt, nodes, 'STRT', this%memoryPath)
  end subroutine allocate_arrays

  !> @brief Copy grid data from IDM into package
  subroutine source_griddata(this)
    ! -- modules
    use SimModule, only: store_error
    use MemoryManagerExtModule, only: mem_set_value
    use GwfIcInputModule, only: GwfIcParamFoundType
    ! -- dummy
    class(GwfIcType) :: this
    ! -- local
    character(len=LINELENGTH) :: errmsg
    type(GwfIcParamFoundType) :: found
    integer(I4B), dimension(:), pointer, contiguous :: map
    !
    ! -- set map to convert user to reduced node data
    map => null()
    if (this%dis%nodes < this%dis%nodesuser) map => this%dis%nodeuser
    !
    ! -- set values
    call mem_set_value(this%strt, 'STRT', this%input_mempath, map, found%strt)
    !
    ! -- ensure STRT was found
    if (.not. found%strt) then
      write (errmsg, '(a)') 'Error in GRIDDATA block: STRT not found.'
      call store_error(errmsg)
    else if (this%iout > 0) then
      write (this%iout, '(4x,a)') 'STRT set from input file'
    end if
  end subroutine source_griddata

end module GwfIcModule
