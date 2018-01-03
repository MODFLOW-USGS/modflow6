module TimeArrayModule

  use BaseDisModule,    only: DisBaseType
  use ConstantsModule,  only: LINELENGTH
  use KindModule,       only: DP, I4B
  use ListModule,       only: ListType
  use SimModule,        only: store_error, ustop

  private
  public :: TimeArrayType, ConstructTimeArray, &
            AddTimeArrayToList, CastAsTimeArrayType, &
            GetTimeArrayFromList

  type :: TimeArrayType
    ! -- Public members
    real(DP),                        public :: taTime
    real(DP), dimension(:), pointer, public :: taArray => null()
  contains
    ! -- Public procedures
    ! -- When gfortran adds support for finalization, the
    !    following declaration could be: final :: finalize
    procedure, public :: da => ta_da
  end type TimeArrayType

contains

  subroutine ConstructTimeArray(newTa, dis)
    ! Allocate and assign members of a new TimeArrayType object.
    ! Allocate space for the array so that this subroutine can be
    ! called repeatedly with the same array (but with different contents).
    implicit none
    ! -- dummy
    type(TimeArrayType), pointer, intent(out) :: newTa
    class(DisBaseType),   pointer, intent(in)  :: dis
    ! -- local
    integer(I4B) :: isize
    character(len=LINELENGTH) :: ermsg
    !
    ! Get dimensions for supported discretization type
    if (dis%supports_layers()) then
      isize = dis%get_ncpl()
    else
      ermsg = 'Time array series is not supported for discretization type'
      call store_error(ermsg)
      call ustop()
    endif
    allocate(newTa)
    allocate(newTa%taArray(isize))
    return
  end subroutine ConstructTimeArray

  function CastAsTimeArrayType(obj) result(res)
    ! Cast an unlimited polymorphic object as TimeArrayType
    implicit none
    ! -- dummy
    class(*), pointer, intent(inout) :: obj
    type(TimeArrayType), pointer :: res
    !
    res => null()
    if (.not. associated(obj)) return
    !
    select type (obj)
    type is (TimeArrayType)
      res => obj
    end select
    return
  end function CastAsTimeArrayType

  subroutine AddTimeArrayToList(list, timearray)
    implicit none
    ! -- dummy
    type(ListType),               intent(inout) :: list
    type(TimeArrayType), pointer, intent(inout) :: timearray
    ! -- local
    class(*), pointer :: obj
    !
    obj => timearray
    call list%Add(obj)
    !
    return
  end subroutine AddTimeArrayToList

  function GetTimeArrayFromList(list, indx) result (res)
    implicit none
    ! -- dummy
    type(ListType), intent(inout) :: list
    integer(I4B),   intent(in)    :: indx
    type(TimeArrayType),  pointer :: res
    ! -- local
    class(*), pointer :: obj
    !
    obj => list%GetItem(indx)
    res => CastAsTimeArrayType(obj)
    !
    return
  end function GetTimeArrayFromList

  subroutine ta_da(this)
    implicit none
    class(TimeArrayType) :: this
    !
    deallocate(this%taArray)
    !
    return
  end subroutine ta_da

end module TimeArrayModule
