module GwtGwtExchangeModule
  use KindModule, only: I4B, LGP
  use ConstantsModule, only: LINELENGTH
  use ListModule, only: ListType
  use InputOutputModule, only: getunit, openfile
  use SimModule, only: store_error
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
  public :: GetGwtExchangeFromList
  public :: CastAsGwtExchange

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
    procedure :: read_options
    procedure :: parse_option
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
  exchange%filename = filename
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
  this%ixt3d = 1

end subroutine allocate_scalars

!> @brief read the data from the input file
!< and store
subroutine gwtgwt_df(this)
  use SimVariablesModule, only: iout
  class(GwtExchangeType) :: this
  ! local
  integer(I4B) :: inunit

  inunit = getunit()
  write(iout,'(/a,a)') ' Creating exchange: ', this%name
  call openfile(inunit, iout, this%filename, 'GWT-GWT')

  call this%parser%Initialize(inunit, iout)

  ! check that models are in same solution
  if(this%gwtmodel1%idsoln /= this%gwtmodel2%idsoln) then
    call store_error('ERROR.  TWO MODELS ARE CONNECTED ' //                  &
      'IN A GWT EXCHANGE BUT THEY ARE IN DIFFERENT SOLUTIONS. ' //           &
      'GWT MODELS MUST BE IN SAME SOLUTION: ' //                             &
      trim(this%gwtmodel1%name) // ' ' // trim(this%gwtmodel2%name) )
    call this%parser%StoreErrorUnit()
  endif
  
  call this%read_options(iout)

  call this%read_dimensions(iout)

  call this%DisConnExchangeType%allocate_arrays()

  call this%read_data(iout)

  ! increase_edge_count equivalent?
  
  close(inunit)

end subroutine gwtgwt_df

!> @brief Read options block from input file
!<
subroutine read_options(this, iout)
  use ConstantsModule, only: LINELENGTH, LENAUXNAME, DEM6
  use MemoryManagerModule, only: mem_allocate
  use SimVariablesModule, only: errmsg
  class(GwtExchangeType) :: this   !< this exchange
  integer(I4B), intent(in) :: iout !< the output file unit
  ! -- local
  character(len=LINELENGTH) :: keyword
  logical :: isfound
  logical :: endOfBlock    
  integer(I4B) :: ierr

  call this%parser%GetBlock('OPTIONS', isfound, ierr,                        &
    supportOpenClose=.true., blockRequired=.false.)
  !
  ! -- parse options block if detected
  if (isfound) then
    write(iout,'(1x,a)')'PROCESSING GWT-GWT EXCHANGE OPTIONS'
    do
      call this%parser%GetNextLine(endOfBlock)
      if (endOfBlock) then
        exit
      end if
      call this%parser%GetStringCaps(keyword)

      ! first parse option in base
      if (this%DisConnExchangeType%parse_option(keyword, iout)) then
        cycle
      end if

      ! it's probably ours
      if (this%parse_option(keyword, iout)) then
        cycle
      end if

      ! unknown option
      errmsg = "Unknown GWT-GWT exchange option '" // trim(keyword) // "'."
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    end do

    write(iout,'(1x,a)') 'END OF GWT-GWT EXCHANGE OPTIONS'
  end if
  
  return
end subroutine read_options

!> @brief parse option from exchange file
  !<
function parse_option(this, keyword, iout) result(parsed)
  use SimVariablesModule, only: errmsg
  class(GwtExchangeType) :: this                   !< instance of exchange object
  character(len=LINELENGTH), intent(in) :: keyword !< the option name
  integer(I4B), intent(in) :: iout                 !< for logging    
  logical(LGP) :: parsed                           !< true when parsed
  ! local
  character(len=LINELENGTH) :: subkey

  parsed = .true.

  select case (keyword)
  case ('ADVSCHEME')
    call this%parser%GetStringCaps(subkey)
    select case(subkey)
    case('UPSTREAM')
      this%iAdvScheme = 0
    case('CENTRAL')
      this%iAdvScheme = 1
    case('TVD')
      this%iAdvScheme = 2
    case default
      errmsg = "Unknown weighting method for advection: '" // trim(subkey) // "'."
      call store_error(errmsg)
      call this%parser%StoreErrorUnit()
    end select

    write(iout,'(4x,a,a)')                                                      &
      'CELL AVERAGING METHOD HAS BEEN SET TO: ', trim(subkey)

  case ('XT3D_OFF')
    this%ixt3d = 0
    write(iout, '(4x,a)') 'XT3D FORMULATION HAS BEEN SHUT OFF.'
  case ('XT3D_RHS')
    this%ixt3d = 2
    write(iout, '(4x,a)') 'XT3D RIGHT-HAND SIDE FORMULATION IS SELECTED.'
  case default
    parsed = .false.
  end select

end function parse_option

!> @brief deallocate members of the exchange object
!<
subroutine gwtgwt_da(this)
  class(GwtExchangeType) :: this

  call mem_deallocate(this%iAdvScheme)

  ! deallocate base
  call this%DisConnExchangeType%disconnex_da()

end subroutine gwtgwt_da

function CastAsGwtExchange(obj) result (res)
  class(*), pointer, intent(inout) :: obj
  class(GwtExchangeType), pointer :: res
  
  res => null()
  if (.not. associated(obj)) return
  
  select type (obj)
  class is (GwtExchangeType)
    res => obj
  end select
  return
end function CastAsGwtExchange

function GetGwtExchangeFromList(list, idx) result (res)
  type(ListType), intent(inout) :: list
  integer(I4B), intent(in)    :: idx
  class(GwtExchangeType), pointer    :: res
  ! -- local
  class(*), pointer :: obj
  
  obj => list%GetItem(idx)
  res => CastAsGwtExchange(obj)
  
  return
end function GetGwtExchangeFromList


end module GwtGwtExchangeModule