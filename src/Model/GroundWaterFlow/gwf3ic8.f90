module GwfIcModule

  use KindModule, only: DP, I4B
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
    procedure :: ic_ar
    procedure :: ic_da
    procedure, private :: allocate_arrays
    procedure :: source_data
  end type GwfIcType

contains

  subroutine ic_cr(ic, name_model, inunit, iout, dis)
! ******************************************************************************
! ic_cr -- Create a new initial conditions object
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use IdmMf6FileLoaderModule, only: input_load
    use ConstantsModule, only: LENPACKAGETYPE
    ! -- dummy
    type(GwfIcType), pointer :: ic
    character(len=*), intent(in) :: name_model
    integer(I4B), intent(in) :: inunit
    integer(I4B), intent(in) :: iout
    class(DisBaseType), pointer, intent(in) :: dis
! ------------------------------------------------------------------------------
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
    ic%inunit = inunit
    ic%iout = iout
    !
    ! -- set pointers
    ic%dis => dis
    !
    ! -- Initialize block parser
    call ic%parser%Initialize(ic%inunit, ic%iout)
    !
    ! -- Load package input context
    call input_load(ic%parser, 'IC6', 'GWF', 'IC', ic%name_model, 'IC', iout)
    !
    ! -- Return
    return
  end subroutine ic_cr

  subroutine ic_ar(this, x)
! ******************************************************************************
! ic_ar -- Allocate and read initial conditions
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use BaseDisModule, only: DisBaseType
    use SimModule, only: store_error
    ! -- dummy
    class(GwfIcType) :: this
    real(DP), dimension(:), intent(inout) :: x
    ! -- locals
    integer(I4B) :: n
! ------------------------------------------------------------------------------
    !
    ! -- Print a message identifying the initial conditions package.
    write (this%iout, 1) this%inunit
1   format(1x, /1x, 'IC -- INITIAL CONDITIONS PACKAGE, VERSION 8, 3/28/2015', &
           ' INPUT READ FROM UNIT ', i0)
    !
    ! -- Allocate arrays
    call this%allocate_arrays(this%dis%nodes)
    !
    ! -- Source data
    call this%source_data()
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
! ******************************************************************************
! ic_da -- Deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    use MemoryManagerExtModule, only: memorylist_remove
    use SimVariablesModule, only: idm_context
    ! -- dummy
    class(GwfIcType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- Deallocate package input context
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

  subroutine allocate_arrays(this, nodes)
! ******************************************************************************
! allocate_arrays
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(GwfIcType) :: this
    integer(I4B), intent(in) :: nodes
    ! -- local
! ------------------------------------------------------------------------------
    !
    ! -- Allocate
    call mem_allocate(this%strt, nodes, 'STRT', this%memoryPath)
    !
    ! -- Return
    return
  end subroutine allocate_arrays

  subroutine source_data(this)
! ******************************************************************************
! source_data -- source package data from input context
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LENMEMPATH
    use MemoryHelperModule, only: create_mem_path
    use MemoryManagerExtModule, only: mem_set_value
    use SimVariablesModule, only: idm_context
    use GwfIcInputModule, only: GwfIcParamFoundType
    ! -- dummy
    class(GwfIcType) :: this
    ! -- local
    character(len=LENMEMPATH) :: idmMemoryPath
    type(GwfIcParamFoundType) :: found
    integer(I4B), dimension(:), pointer, contiguous :: map
    ! -- formats
! ------------------------------------------------------------------------------
    !
    ! -- initialize map
    map => null()
    !
    ! -- set input context memory path
    idmMemoryPath = create_mem_path(this%name_model, 'IC', idm_context)
    !
    ! -- if reduced, set node map
    if (this%dis%nodes < this%dis%nodesuser) map => this%dis%nodeuser
    !
    ! -- source data from input context
    call mem_set_value(this%strt, 'STRT', idmMemoryPath, map, found%strt)
    !
    ! -- Return
    return
  end subroutine source_data

end module GwfIcModule
