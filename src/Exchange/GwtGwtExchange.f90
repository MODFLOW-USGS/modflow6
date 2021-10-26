module GwtGwtExchangeModule
  use KindModule, only: I4B, LGP
  use ListModule, only: ListType
  use MemoryManagerModule, only: mem_allocate, mem_deallocate
  use MemoryHelperModule, only: create_mem_path
  use BaseExchangeModule, only: BaseExchangeType, AddBaseExchangeToList
  use DisConnExchangeModule
  use ListsModule, only: baseexchangelist, basemodellist
  use BaseModelModule, only: BaseModelType, GetBaseModelFromList
  use GwtModule, only: GwtModelType

  implicit none
  private

  public :: gwtexchange_create
  public :: GwtExchangeType
  public :: GetGwfExchangeFromList

  !> Data structure to hold the spatial connection data
  !! between two GWT models. The logic to get the coefficients
  !! to the solution will be in the GwtGwtConnection.
  !<
  type, extends(DisConnExchangeType) :: GwtExchangeType

    class(GwtModelType), pointer :: gwtmodel1 !< one transport model in the exchange
    class(GwtModelType), pointer :: gwtmodel2 !< the other transport model in the exchange

    integer(I4B), pointer :: iAdvScheme       !< the advection scheme at the interface:
                                              !! 0 = upstream, 1 = central, 2 = TVD

  contains
    procedure :: exg_df => gwtgwt_df
    procedure :: exg_da => gwtgwt_da
    procedure :: connects_model => gwtgwt_connects_model
    procedure :: use_interface_model
    procedure :: allocate_scalars

  end type GwtExchangeType

contains

!> @brief Create the GWT-GWT exchange data structure
!<
subroutine gwtexchange_create(filename, id, m1id, m2id)
  character(len=*),intent(in) :: filename !< the input file for this exchange
  integer(I4B), intent(in) :: id          !< the numbered id for this exchange
  integer(I4B), intent(in) :: m1id        !< model number one
  integer(I4B), intent(in) :: m2id        !< model number two
  ! -- local
  type(GwtExchangeType), pointer :: exchange
  class(BaseExchangeType), pointer :: baseExchange
  class(BaseModelType), pointer :: mb
  character(len=20) :: cint

  allocate(exchange)
  baseExchange => exchange
  call AddBaseExchangeToList(baseexchangelist, baseExchange)

  ! assign id and name
  exchange%id = id
  write(cint, '(i0)') id
  exchange%name = 'GWT-GWT_' // trim(adjustl(cint))
  exchange%memoryPath = create_mem_path(exchange%name)
  
  ! -- allocate scalars and set defaults
  call exchange%allocate_scalars()
  exchange%typename = 'GWT-GWT'
  !
  ! set models
  mb => GetBaseModelFromList(basemodellist, m1id)    
  select type (mb)
  type is (GwtModelType)
    exchange%model1 => mb
    exchange%gwtmodel1 => mb
  end select
  mb => GetBaseModelFromList(basemodellist, m2id)
  select type (mb)
  type is (GwtModelType)
    exchange%model2 => mb
    exchange%gwtmodel2 => mb
  end select

end subroutine gwtexchange_create

!> @brief Return true when this exchange provides matrix 
!< coefficients for solving @param model
function gwtgwt_connects_model(this, model) result(is_connected)
  class(GwtExchangeType) :: this                      !< the instance of the GWT-GWT exchange
  class(BaseModelType), pointer, intent(in) :: model  !< the model to which the exchange might hold a connection
  logical(LGP) :: is_connected                        !< true, when connected

  is_connected = .false.
  
  select type(model)
  class is (GwtModelType)    
  if (associated(this%gwtmodel1, model)) then
    is_connected = .true.
  else if (associated(this%gwtmodel2, model)) then
    is_connected = .true.
  end if    
  end select

end function gwtgwt_connects_model

!> @brief Should interface model be used for this exchange
!<
function use_interface_model(this) result(useIM)
  class(GwtExchangeType) :: this !< instance of exchange object
  logical(LGP) :: useIM          !< true when interface model should be used

  ! transport always uses IM for this coupling
  useIM = .true.

end function

!> @brief allocate the scalar variables for this exchange
!<
subroutine allocate_scalars(this)
  class(GwtExchangeType) :: this !< this GWT exchange

  call this%DisConnExchangeType%allocate_scalars()
  call mem_allocate(this%iAdvScheme, 'IADVSCHEME', this%memoryPath)

  this%iAdvScheme = 0

end subroutine allocate_scalars

!> @brief read the data from the input file
!< and store
subroutine gwtgwt_df(this)
  class(GwtExchangeType) :: this

  call this%DisConnExchangeType%allocate_arrays()

end subroutine gwtgwt_df

!> @brief deallocate members of the exchange object
!<
subroutine gwtgwt_da(this)
  class(GwtExchangeType) :: this

  call mem_deallocate(this%iAdvScheme)

  ! deallocate base
  call this%DisConnExchangeType%disconnex_da()

end subroutine gwtgwt_da

function CastAsGwtExchangeClass(obj) result (res)
  class(*), pointer, intent(inout) :: obj
  class(GwtExchangeType), pointer :: res
  
  res => null()
  if (.not. associated(obj)) return
  
  select type (obj)
  class is (GwtExchangeType)
    res => obj
  end select
  return
end function CastAsGwtExchangeClass

function GetGwfExchangeFromList(list, idx) result (res)
  type(ListType), intent(inout) :: list
  integer(I4B), intent(in)    :: idx
  class(GwtExchangeType), pointer    :: res
  ! -- local
  class(*), pointer :: obj
  
  obj => list%GetItem(idx)
  res => CastAsGwtExchangeClass(obj)
  
  return
end function GetGwfExchangeFromList


end module GwtGwtExchangeModule