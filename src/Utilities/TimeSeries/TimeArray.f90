module TimeArrayModule

  use KindModule, only: DP, I4B
  use ListModule, only: ListType
  use SimVariablesModule, only: errmsg
  use SimModule, only: store_error

  implicit none
  private
  public :: TimeArrayType, ConstructTimeArray, &
            AddTimeArrayToList, CastAsTimeArrayType, &
            GetTimeArrayFromList

  type :: TimeArrayType
    ! -- Public members
    real(DP), public :: taTime
    real(DP), dimension(:), pointer, contiguous, public :: taArray => null()
  contains
    ! -- Public procedures
    ! -- When gfortran adds support for finalization, the
    !    following declaration could be: final :: finalize
    procedure, public :: da => ta_da
  end type TimeArrayType

contains

  subroutine ConstructTimeArray(newTa, modelname)
! ******************************************************************************
! ConstructTimeArray -- construct time array
!   Allocate and assign members of a new TimeArrayType object.
!   Allocate space for the array so that this subroutine can be
!   called repeatedly with the same array (but with different contents).
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    use ConstantsModule, only: LENMEMPATH
    use MemoryManagerModule, only: mem_setptr
    use MemoryHelperModule, only: create_mem_path
    ! -- dummy
    type(TimeArrayType), pointer, intent(out) :: newTa
    character(len=*), intent(in) :: modelname
    ! -- local
    integer(I4B), dimension(:), contiguous, &
      pointer :: mshape
    character(len=LENMEMPATH) :: mempath
    integer(I4B) :: isize
! ------------------------------------------------------------------------------
    !
    ! -- initialize
    nullify (mshape)
    !
    ! -- create mempath
    mempath = create_mem_path(component=modelname, subcomponent='DIS')
    !
    ! -- set mshape pointer
    call mem_setptr(mshape, 'MSHAPE', mempath)
    !
    ! Get dimensions for supported discretization type
    if (size(mshape) == 2) then
      isize = mshape(2)
    else if (size(mshape) == 3) then
      isize = mshape(2) * mshape(3)
    else
      errmsg = 'Time array series is not supported for discretization type'
      call store_error(errmsg, terminate=.TRUE.)
    end if
    !
    allocate (newTa)
    allocate (newTa%taArray(isize))
    return
  end subroutine ConstructTimeArray

  function CastAsTimeArrayType(obj) result(res)
! ******************************************************************************
! ConstructTimeArray -- Cast an unlimited polymorphic object as TimeArrayType
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(*), pointer, intent(inout) :: obj
    type(TimeArrayType), pointer :: res
! ------------------------------------------------------------------------------
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
! ******************************************************************************
! AddTimeArrayToList -- add ta to list
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(ListType), intent(inout) :: list
    type(TimeArrayType), pointer, intent(inout) :: timearray
    ! -- local
    class(*), pointer :: obj
! ------------------------------------------------------------------------------
    !
    obj => timearray
    call list%Add(obj)
    !
    return
  end subroutine AddTimeArrayToList

  function GetTimeArrayFromList(list, indx) result(res)
! ******************************************************************************
! GetTimeArrayFromList -- get ta from list
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    type(ListType), intent(inout) :: list
    integer(I4B), intent(in) :: indx
    type(TimeArrayType), pointer :: res
    ! -- local
    class(*), pointer :: obj
! ------------------------------------------------------------------------------
    !
    obj => list%GetItem(indx)
    res => CastAsTimeArrayType(obj)
    !
    return
  end function GetTimeArrayFromList

  subroutine ta_da(this)
! ******************************************************************************
! ta_da -- deallocate
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(TimeArrayType) :: this
! ------------------------------------------------------------------------------
    !
    deallocate (this%taArray)
    this%taArray => null()
    !
    return
  end subroutine ta_da

end module TimeArrayModule
