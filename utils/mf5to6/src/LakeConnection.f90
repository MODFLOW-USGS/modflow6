module LakeConnectionModule
  
  use ConstantsModule, only: DZERO
  use GLOBAL, only: BOTM, NLAY, NROW, NCOL
  use GWFLAKMODULE, only: LKARR1
  use ListModule, only: ListType
  
  implicit none
  
  private
  public :: LakeConnectionType, ConstructLakeConnection, &
            CastAsLakeConnectionType, AddLakeConnectionToList, &
            GetConnectionFromList
  
  type LakeConnectionType
    integer           :: LakeNum = 0
    integer           :: Iconn = 0
    integer           :: Irow = 0
    integer           :: Jcol = 0
    integer           :: Klay = 0
    character(len=10) :: CLakType = ''
    double precision  :: BedLeak = DZERO
    double precision  :: Belev = DZERO     ! Leave as zero
    double precision  :: Telev = DZERO     ! Leave as zero
    double precision  :: ConnLen = DZERO   ! For horiz. connection, = 0.5 delr or delc
    double precision  :: ConnWidth = DZERO ! For horiz. connection, = delr or delc
  contains
    procedure, private :: FindBottomElev
  end type LakeConnectionType

contains

  ! Type-bound procedures
  subroutine FindBottomElev(this)
    ! dummy
    class(LakeConnectionType) :: this
    !
    ! Ned todo: find lake bottom elevation, given irow and jcol, LKARR1 and BOTM arrays
    !
    return
  end subroutine FindBottomElev

  ! Non-type-bound procedures

  subroutine ConstructLakeConnection(newLakeConnection, lakeNum, iconn, &
                                     irow, jcol, klay, claktype)
    ! dummy
    type(LakeConnectionType), pointer :: newLakeConnection
    integer, intent(in) :: lakeNum, iconn, irow, jcol, klay
    character(len=*), intent(in) :: claktype
    !
    allocate(newLakeConnection)
    newLakeConnection%LakeNum = lakeNum
    newLakeConnection%Iconn = iconn
    newLakeConnection%Irow = irow
    newLakeConnection%Jcol = jcol
    newLakeConnection%Klay = klay
    newLakeConnection%CLakType = claktype
    !
    return
  end subroutine ConstructLakeConnection
  
  function CastAsLakeConnectionType(obj) result (res)
    ! dummy
    class(*), pointer, intent(inout) :: obj
    type(LakeConnectionType), pointer :: res
    !
    res => null()
    select type (obj)
    type is (LakeConnectionType)
      res => obj
    end select
    !
    return
  end function CastAsLakeConnectionType
  
  subroutine AddLakeConnectionToList(list, lakeConnection)
    implicit none
    ! dummy
    type(ListType), pointer :: list
    type(LakeConnectionType), pointer :: lakeConnection
    ! local
    class(*), pointer :: obj => null()
    !
    obj => lakeConnection
    call list%Add(obj)
    !
    return
  end subroutine AddLakeConnectionToList

  function GetConnectionFromList(list, idx) result (res)
    implicit none
    ! dummy
    type(ListType), pointer :: list
    integer, intent(in) :: idx
    type(LakeConnectionType), pointer :: res
    ! local
    class(*), pointer :: obj => null()
    !
    obj => list%GetItem(idx)
    res => CastAsLakeConnectionType(obj)
    !
    return
  end function GetConnectionFromList

end module LakeConnectionModule
