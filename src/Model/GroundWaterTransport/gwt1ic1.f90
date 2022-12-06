module GwtIcModule

  use KindModule, only: DP, I4B
  use GwfIcModule, only: GwfIcType
  use BlockParserModule, only: BlockParserType
  use BaseDisModule, only: DisBaseType

  implicit none
  private
  public :: GwtIcType
  public :: ic_cr

  ! -- Most of the GwtIcType functionality comes from GwfIcType
  type, extends(GwfIcType) :: GwtIcType
  contains
    procedure :: source_data
  end type GwtIcType

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
    type(GwtIcType), pointer :: ic
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
    call input_load(ic%parser, 'IC6', 'GWT', 'IC', ic%name_model, 'IC', iout)
    !
    ! -- Return
    return
  end subroutine ic_cr

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
    use GwtIcInputModule, only: GwtIcParamFoundType
    ! -- dummy
    class(GwtIcType) :: this
    ! -- local
    character(len=LENMEMPATH) :: idmMemoryPath
    type(GwtIcParamFoundType) :: found
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

end module GwtIcModule
