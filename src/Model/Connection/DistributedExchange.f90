module DistributedExchangeModule
  use KindModule, only: I4B
  use ConstantsModule, only: LENEXCHANGENAME
  use ListModule
  use SimModule, only: ustop
  use DisConnExchangeModule
  use ListsModule, only: baseexchangelist, distexchangelist
  implicit none
  private

  public :: DistributedExchangeType
  public :: add_dist_exg, get_dist_exg

  type :: DistributedExchangeType
    integer(I4B) :: id !< universal identifier: id of the exchange
    character(len=LENEXCHANGENAME) :: name !< exchangename
    integer(I4B) :: model1_id !< one model
    integer(I4B) :: model2_id !< other model

    ! this is strictly private, use access() instead
    class(DisConnExchangeType), private, pointer :: exchange !< implementation if local, null otherwise
  contains    
    procedure, private :: create_local
    procedure :: access
  end type

  contains  

  subroutine add_dist_exg(exg_index)
    integer(I4B) :: exg_index
    ! local
    class(DisConnExchangeType), pointer :: exchange
    class(DistributedExchangeType), pointer :: dist_exchange

    exchange => GetDisConnExchangeFromList(baseexchangelist, exg_index)
    
    allocate(dist_exchange)
    call dist_exchange%create_local(exchange)

    call AddDistExchangeToList(distexchangelist, dist_exchange)

  end subroutine add_dist_exg

  subroutine create_local(this, exchange)
    class(DistributedExchangeType) :: this
    class(DisConnExchangeType), pointer :: exchange

    this%id = exchange%id
    this%name = exchange%name
    this%model1_id = exchange%model1%id
    this%model2_id = exchange%model2%id
    this%exchange => exchange

  end subroutine create_local

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