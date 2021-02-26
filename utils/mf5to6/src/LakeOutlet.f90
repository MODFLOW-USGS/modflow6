! LAK3 does not support lake outflow directly to another lake.
!
! Outlets in LAK8 allow flow from a lake (lakein) to an external
! boundary (lakeout = 0).  
! SFR7 input defines this link in IDIVAR(1,*) and LAK3 stores it
! in IDIV(LAKE,K1) (see SGWF2LAK7SFR7RPS).
! Need to know how SFR7 determines flow rate (or stage in stream?)
! SFR7 calculates outflow from lake based on lake stage and
! ICALC etc. of SFR segment.
! So: Outlet members cOutType, Width, Rough, Slope and maybe dInvert
! will need to come from SFR input (ICALC, ROUGHCH, etc.)
! ICALC = 1 -- MANNING.  Not sure other values of ICALC have 
! equivalent in MF6.
!
! QP (provider flow rate for water mover) means different things for
! different packages. For LAK, QP is the combined outflow rate from
! all of its discharge points, if there are any. 
! I assume "discharge point" = "outlet".  According to Joe (5/10/16),
! QP is for one outlet, and for a LAK->SFR mover, the ID on the LAK
! side should be the outlet number.
!
! Use water movers to move water from a stream to a lake.
! SFR7 input defines this link in IOTSEG (OUTSEG), and LAK3
! stores it in ITRB(LAKE,K1) (see SGWF2LAK7SFR7RPS).

module LakeOutletModule

  use ConstantsModule, only: DZERO, LENPACKAGENAME
  use ListModule, only: ListType
  
  private
  public :: LakeOutletType, ConstructLakeOutlet, &
            CastAsLakeOutletType, AddLakeOutletToList, &
            GetOutletFromList
  
  type LakeOutletType
    integer                       :: iOutletNum = 0
    integer                       :: LakeIn = 0
    integer                       :: LakeOut = 0
    integer                       :: iSegnum = 0
    integer                       :: IgridRcvr = 0
    character(len=10)             :: cOutType = ''
    character(len=LENPACKAGENAME) :: rcvrPkgName = ''
    double precision              :: dInvert = DZERO
    double precision              :: Width = DZERO
    double precision              :: Rough = DZERO
    double precision              :: Slope = DZERO
  end type LakeOutletType
  
contains

  ! Non-type-bound procedures

  subroutine ConstructLakeOutlet(newLakeOutlet)
    ! dummy
    type(LakeOutletType), pointer :: newLakeOutlet
    !
    allocate(newLakeOutlet)
    !
    return
  end subroutine ConstructLakeOutlet
  
  function CastAsLakeOutletType(obj) result (res)
    ! dummy
    class(*), pointer, intent(inout) :: obj
    type(LakeOutletType), pointer :: res
    !
    res => null()
    select type (obj)
    type is (LakeOutletType)
      res => obj
    end select
    !
    return
  end function CastAsLakeOutletType

  subroutine AddLakeOutletToList(list, lakeOutlet)
    implicit none
    ! dummy
    type(ListType), pointer :: list
    type(LakeOutletType), pointer :: lakeOutlet
    ! local
    class(*), pointer :: obj => null()
    !
    obj => lakeOutlet
    call list%Add(obj)
    !
    return
  end subroutine AddLakeOutletToList

  function GetOutletFromList(list, idx) result (res)
    implicit none
    ! dummy
    type(ListType), pointer :: list
    integer, intent(in) :: idx
    type(LakeOutletType), pointer :: res
    ! local
    class(*), pointer :: obj => null()
    !
    obj => list%GetItem(idx)
    res => CastAsLakeOutletType(obj)
    !
    return
  end function GetOutletFromList

end module LakeOutletModule
