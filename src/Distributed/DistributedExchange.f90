module DistributedExchangeModule
  use KindModule, only: I4B, DP
  use ListModule
  use SimModule, only: ustop
  use DisConnExchangeModule
  use ListsModule, only: baseexchangelist
  use DistListsModule, only: distexchangelist
  use DistributedBaseModule
  implicit none
  private

  public :: DistributedExchangeType
  public :: add_dist_exg, get_dist_exg
  public :: GetDistExchangeFromList

  type, extends(DistributedBaseType) :: DistributedExchangeType
    integer(I4B) :: model1_id !< one model
    integer(I4B) :: model2_id !< other model

    ! nexg*      
    ! nodem1*,nodem2*
    ! naux,auxname_cst,auxvar
    integer(I4B), pointer :: nexg => null()
    integer(I4B), dimension(:), pointer, contiguous :: nodem1 => null()
    integer(I4B), dimension(:), pointer, contiguous :: nodem2 => null()
    integer(I4B), pointer :: ianglex => null()
    integer(I4B), dimension(:), pointer, contiguous :: ihc => null()
    real(DP), dimension(:), pointer, contiguous :: cl1 => null()
    real(DP), dimension(:), pointer, contiguous :: cl2 => null()
    real(DP), dimension(:), pointer, contiguous :: hwva => null()
    real(DP), dimension(:), pointer, contiguous :: anglex => null() ! TODO_MJR: this doesn't exist yet in the exchanges

    ! this is strictly private, use access() instead
    class(DisConnExchangeType), private, pointer :: exchange !< implementation if local, null otherwise
  contains    
    procedure :: create
    procedure :: init_connectivity
    procedure :: deallocate
    procedure :: access
  end type

  contains  

  subroutine add_dist_exg(exg_id, model1_id, model2_id)
    integer(I4B) :: exg_id
    integer(I4B) :: model1_id
    integer(I4B) :: model2_id
    ! local
    class(DisConnExchangeType), pointer :: exchange
    class(DistributedExchangeType), pointer :: dist_exchange

    exchange => GetDisConnExchangeFromList(baseexchangelist, exg_id)
    
    allocate(dist_exchange)
    call dist_exchange%create(exchange, exg_id, model1_id, model2_id)
    call AddDistExchangeToList(distexchangelist, dist_exchange)

  end subroutine add_dist_exg

  subroutine create(this, exchange, exchange_id, m1_id, m2_id)
    class(DistributedExchangeType) :: this
    class(DisConnExchangeType), pointer :: exchange    
    integer(I4B) :: exchange_id
    integer(I4B) :: m1_id
    integer(I4B) :: m2_id

    this%id = exchange_id
    this%model1_id = m1_id
    this%model2_id = m2_id
    this%exchange => exchange

    if (associated(exchange)) then
      this%is_local = .true.      
    else
      this%is_local = .false.
    end if

  end subroutine create

  subroutine init_connectivity(this)
    class(DistributedExchangeType) :: this

    if (this%is_local) then
      
    end if

  end subroutine init_connectivity

  subroutine deallocate(this)
    class(DistributedExchangeType) :: this
  end subroutine deallocate

  function access(this) result(exchange)
    class(DistributedExchangeType) :: this
    class(DisConnExchangeType), pointer :: exchange

    if (associated(this%exchange)) then
      exchange => this%exchange
    else
      write (*, *) 'Error: illegal access to remote memory, abort'
      call ustop()
    end if

  end function access

  !> @brief Gets the distributed exchange structure for
  !! a specific exchange id
  !<
  function get_dist_exg(exchange_id) result(dist_exg)
    integer(I4B) :: exchange_id !< id of the actual exchange
    class(DistributedExchangeType), pointer :: dist_exg !< the distributed exchange
    ! local
    integer(I4B) :: i
    class(DistributedExchangeType), pointer :: dxg

    dist_exg => null()
    do i = 1, distexchangelist%Count()
      dxg => GetDistExchangeFromList(distexchangelist, i)
      if (dxg%id == exchange_id) then
        dist_exg => dxg
        return
      end if
    end do

  end function get_dist_exg

  function CastAsDistExchangeClass(obj) result(res)
    class(*), pointer, intent(inout) :: obj
    class(DistributedExchangeType), pointer :: res

    res => null()
    if (.not. associated(obj)) return

    select type (obj)
    class is (DistributedExchangeType)
      res => obj
    end select
    return

  end function CastAsDistExchangeClass

  subroutine AddDistExchangeToList(list, exg)
    type(ListType), intent(inout) :: list
    class(DistributedExchangeType), pointer, intent(inout) :: exg
    ! local
    class(*), pointer :: obj

    obj => exg
    call list%Add(obj)
    return

  end subroutine AddDistExchangeToList

  function GetDistExchangeFromList(list, idx) result(res)
    type(ListType), intent(inout) :: list
    integer(I4B), intent(in) :: idx
    class(DistributedExchangeType), pointer :: res
    ! local
    class(*), pointer :: obj

    obj => list%GetItem(idx)
    res => CastAsDistExchangeClass(obj)
    return

  end function GetDistExchangeFromList

end module